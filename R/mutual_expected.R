#' Calculate expected values when true segregation is zero
#'
#' When sample sizes are small, one group has a small proportion, or
#' when there are many units, segregation indices are typically upwardly
#' biased, even when true segregation is zero. This function simulates
#' tables with zero segregation, given the marginals of the dataset,
#' and calculates segregation. If the expected values are large,
#' the interpretation of index scores might have to be adjusted.
#'
#' @param data A data frame.
#' @param group A categorical variable or a vector of variables
#'   contained in \code{data}. Defines the first dimension
#'   over which segregation is computed.
#' @param unit A categorical variable or a vector of variables
#'   contained in \code{data}. Defines the second dimension
#'   over which segregation is computed.
#' @param weight Numeric. (Default \code{NULL})
#' @param fixed_margins Should the margins be fixed or simulated? (Default \code{TRUE})
#' @param n_bootstrap Number of bootstrap iterations. (Default \code{100})
#' @param base Base of the logarithm that is used in the calculation.
#'   Defaults to the natural logarithm.
#' @return A data.table with two rows, corresponding to the expected values of
#'    segregation when true segregation is zero.
#' @examples
#' # the schools00 dataset has a large sample size, so expected segregation is close to zero
#' mutual_expected(schools00, "race", "school", weight = "n")
#'
#' # but we can build a smaller table, with 100 students distributed across
#' # 10 schools, where one racial group has 10% of the students
#' small <- data.frame(
#'           school = c(1:10, 1:10),
#'           race = c(rep("r1", 10), rep("r2", 10)),
#'           n = c(rep(1, 10), rep(9, 10)))
#' mutual_expected(small, "race", "school", weight= "n")
#' # with an increase in sample size (n=1000), the values improve
#' small$n <- small$n * 10
#' mutual_expected(small, "race", "school", weight= "n")
#' @import data.table
#' @export
mutual_expected <- function(data, group, unit, weight = NULL,
                            fixed_margins = TRUE,
                            n_bootstrap = 100, base = exp(1)) {
    d <- prepare_data(data, group, unit, weight)
    n_total <- d[, sum(freq)]
    if (all.equal(n_total, round(n_total)) == FALSE) {
        stop(paste0(
            "bootstrap with a total sample size that is not an integer is not allowed, ",
            "maybe scale your weights?"))
    }

    n_group <- d[, sum(freq), by = get(group)][, V1]
    n_unit <- d[, sum(freq), by = get(unit)][, V1]
    p_group <- n_group / sum(n_group)
    p_unit <- n_unit / sum(n_unit)

    if (fixed_margins == TRUE) {
        # take the margins from the table provided
        entropy_group <- -sum(p_group * logf(p_group, base))
        entropy_unit <- -sum(p_unit * logf(p_unit, base))
        sim_fixed <- stats::r2dtable(n_bootstrap, n_group, n_unit)
    } else {
        # simulate margins using a multinomial model
        group_margins <- stats::rmultinom(n_bootstrap, sum(n_group), p_group)
        unit_margins <- stats::rmultinom(n_bootstrap, sum(n_unit), p_unit)
    }

    boot_ret <- sapply(seq_len(n_bootstrap), function(i) {
        if (i %% 5 == 0) update_log(bs_n = i, bs_max = n_bootstrap)

        if (fixed_margins == TRUE) {
            p <- sim_fixed[[i]] / sum(sim_fixed[[i]])
        } else {
            sim <- stats::r2dtable(1, group_margins[,i], unit_margins[,i])[[1]]
            p <- sim / sum(sim)
            p_group_sim <- apply(p, 1, sum)
            p_unit_sim <- apply(p, 2, sum)
            entropy_group <- -sum(p_group_sim * logf(p_group_sim, base))
            entropy_unit <- -sum(p_unit_sim * logf(p_unit_sim, base))
        }

        m <- sum(p * logf(p, base)) + entropy_group + entropy_unit
        h <- m / entropy_group
        c(m, h)
    })
    close_log()
    mh <- apply(boot_ret, 1, mean)
    sd <- apply(boot_ret, 1, sd)
    data.table(stat = c("M under 0", "H under 0"), est = mh, se = sd)
}
