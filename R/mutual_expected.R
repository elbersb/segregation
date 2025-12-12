expected_compute <- function(index, d, group_var, unit_var,
                             fixed_margins, n_bootstrap, base) {
    n_group <- d[, sum(freq), by = group_var][, V1]
    by_unit <- d[, list(n = sum(freq)), by = unit_var] # to keep the order
    n_unit <- by_unit[, n]
    if (length(n_group) == 1 || length(n_unit) == 1) {
        if (index == "mh") {
            return(data.table(
                stat = c("M under 0", "H under 0"),
                est = c(NA_real_, NA_real_), se = c(NA_real_, NA_real_)
            ))
        } else {
            return(data.table(
                stat = "D under 0",
                est = NA_real_, se = NA_real_
            ))
        }
    }
    p_group <- n_group / sum(n_group)
    p_unit <- n_unit / sum(n_unit)

    if (fixed_margins == TRUE) {
        # take the margins from the table provided
        entropy_group <- -sum(p_group * logf(p_group, base))
        sim_fixed <- stats::r2dtable(n_bootstrap, n_group, n_unit)
    } else {
        # simulate margins using a multinomial model
        group_margins <- stats::rmultinom(n_bootstrap, sum(n_group), p_group)
        unit_margins <- stats::rmultinom(n_bootstrap, sum(n_unit), p_unit)
    }

    boot_ret <- lapply(seq_len(n_bootstrap), function(i) {
        if (i %% 5 == 0) update_log(bs_n = i, bs_max = n_bootstrap)

        if (index %in% c("ls", "mh")) {
            if (fixed_margins == TRUE) {
                p <- sim_fixed[[i]] / sum(sim_fixed[[i]])
                # margins are fixed, just copy over
                p_group_sim <- p_group
                p_unit_sim <- p_unit
                entropy_group_sim <- entropy_group
            } else {
                sim <- stats::r2dtable(1, group_margins[, i], unit_margins[, i])[[1]]
                p <- sim / sum(sim)
                # margins are simulated, calculate new margins
                p_group_sim <- rowSums(p)
                p_unit_sim <- colSums(p)
                entropy_group_sim <- -sum(p_group_sim * logf(p_group_sim, base))
            }

            # calculate local segregation scores per unit
            p_within_unit <- sweep(p, 2, p_unit_sim, "/")
            ls <- apply(p_within_unit, 2, function(x) sum(x * logf(x / p_group_sim, base)))

            if (index == "mh") {
                m <- sum(p_unit_sim * ls)
                h <- m / entropy_group_sim
                data.table(
                    stat = c("M under 0", "H under 0"),
                    est = c(m, h)
                )
            } else {
                ls_data <- copy(by_unit)
                ls_data[, n := NULL]
                ls_data[, ls := ls]
                ls_data[, p := p_unit_sim]
                ls_data
            }
        } else {
            if (fixed_margins == TRUE) {
                tab <- sim_fixed[[i]]
            } else {
                tab <- stats::r2dtable(1, group_margins[, i], unit_margins[, i])[[1]]
            }

            div <- sweep(tab, 1, rowSums(tab), "/")
            d <- 1 / 2 * sum(apply(div, 2, abs_diff))
            data.table(
                stat = c("D under 0"),
                est = d
            )
        }
    })
    close_log()

    res <- data.table::rbindlist(boot_ret)
    if (index == "ls") {
        res[, .(stat = "LS under 0", est = mean(ls), se = stats::sd(ls), p_mean = mean(p)), by = unit_var]
    } else {
        res[, list(est = mean(est), se = stats::sd(est)), by = .(stat)]
    }
}

#' Calculates expected values when true segregation is zero
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
#' @param within Apply algorithm within each group defined by this variable,
#'   and report the weighted average. (Default \code{NULL})
#' @param fixed_margins Should the margins be fixed or simulated? (Default \code{TRUE})
#' @param n_bootstrap Number of bootstrap iterations. (Default \code{100})
#' @param base Base of the logarithm that is used in the calculation.
#'   Defaults to the natural logarithm.
#' @return A data.table with two rows, corresponding to the expected values of
#'    segregation when true segregation is zero.
#' @examples
#' \dontrun{
#' # the schools00 dataset has a large sample size, so expected segregation is close to zero
#' mutual_total_expected(schools00, "race", "school", weight = "n")
#'
#' # but we can build a smaller table, with 100 students distributed across
#' # 10 schools, where one racial group has 10% of the students
#' small <- data.frame(
#'     school = c(1:10, 1:10),
#'     race = c(rep("r1", 10), rep("r2", 10)),
#'     n = c(rep(1, 10), rep(9, 10))
#' )
#' mutual_total_expected(small, "race", "school", weight = "n")
#' # with an increase in sample size (n=1000), the values improve
#' small$n <- small$n * 10
#' mutual_total_expected(small, "race", "school", weight = "n")
#' }
#' @import data.table
#' @export
mutual_total_expected <- function(data, group, unit, weight = NULL,
                                  within = NULL, fixed_margins = TRUE,
                                  n_bootstrap = 100, base = exp(1)) {
    d <- prepare_data(data, group, unit, weight, within)
    n_total <- d[, sum(freq)]
    if (all.equal(n_total, round(n_total)) != TRUE) {
        stop(paste0(
            "bootstrap with a total sample size that is not an integer is not allowed, ",
            "maybe scale your weights?"
        ))
    }

    if (is.null(within)) {
        res <- expected_compute(
            "mh", d, group, unit,
            fixed_margins, n_bootstrap, base
        )
    } else {
        res <- d[, expected_compute(
            "mh", .SD, ..group, ..unit,
            ..fixed_margins, ..n_bootstrap, ..base
        ),
        by = within
        ]
        n_na <- sum(is.na(res$se)) / 2
        if (n_na > 0) {
            message(paste0("Removed ", n_na, " singleton items"))
            res <- res[!is.na(est)]
        }
        p <- d[, .(p = sum(freq)), by = within]
        res <- merge(res, p, all.x = TRUE, by = within)
        res[, p := p / sum(p), by = .(stat)]
        res <- res[, .(est = sum(est * p), se = sqrt(sum(p * se^2))), by = .(stat)]
    }
    res
}

#' Calculates expected values when true segregation is zero
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
#' @return A data.table with one row, corresponding to the expected value of
#'    the D index when true segregation is zero.
#' @examples
#' # build a smaller table, with 100 students distributed across
#' # 10 schools, where one racial group has 10% of the students
#' small <- data.frame(
#'     school = c(1:10, 1:10),
#'     race = c(rep("r1", 10), rep("r2", 10)),
#'     n = c(rep(1, 10), rep(9, 10))
#' )
#' dissimilarity_expected(small, "race", "school", weight = "n")
#' # with an increase in sample size (n=1000), the values improve
#' small$n <- small$n * 10
#' dissimilarity_expected(small, "race", "school", weight = "n")
#' @import data.table
#' @export
dissimilarity_expected <- function(data, group, unit, weight = NULL,
                                   fixed_margins = TRUE,
                                   n_bootstrap = 100) {
    if (length(unique(data[[group]])) != 2) {
        stop("The D index only allows two distinct groups")
    }
    d <- prepare_data(data, group, unit, weight)

    n_total <- d[, sum(freq)]
    if (all.equal(n_total, round(n_total)) != TRUE) {
        stop(paste0(
            "bootstrap with a total sample size that is not an integer is not allowed, ",
            "maybe scale your weights?"
        ))
    }

    expected_compute(
        "d", d, group, unit,
        fixed_margins, n_bootstrap, exp(1)
    )
}

#' Calculates expected local segregation scores when true segregation is zero
#'
#' When sample sizes are small, one group has a small proportion, or
#' when there are many units, segregation indices are typically upwardly
#' biased, even when true segregation is zero. This function simulates
#' tables with zero segregation, given the marginals of the dataset,
#' and calculates local segregation scores. If the expected values are large,
#' the interpretation of index scores might have to be adjusted.
#'
#' @param data A data frame.
#' @param group A categorical variable or a vector of variables
#'   contained in \code{data}. Defines the first dimension
#'   over which segregation is computed.
#' @param unit A categorical variable or a vector of variables
#'   contained in \code{data}. Defines the group for which local
#'   segregation indices are calculated.
#' @param weight Numeric. (Default \code{NULL})
#' @param fixed_margins Should the margins be fixed or simulated? (Default \code{TRUE})
#' @param n_bootstrap Number of bootstrap iterations. (Default \code{100})
#' @param base Base of the logarithm that is used in the calculation.
#'   Defaults to the natural logarithm.
#' @return A data.table with two rows, corresponding to the expected values of
#'    segregation when true segregation is zero.
#' @examples
#' \dontrun{
#' # the schools00 dataset has a large sample size, so expected segregation is close to zero
#' mutual_local_expected(schools00, "race", "school", weight = "n")
#'
#' # but we can build a smaller table, with 100 students distributed across
#' # 10 schools, where one racial group has 10% of the students
#' small <- data.frame(
#'     school = c(1:10, 1:10),
#'     race = c(rep("r1", 10), rep("r2", 10)),
#'     n = c(rep(1, 10), rep(9, 10))
#' )
#' mutual_local_expected(small, "race", "school", weight = "n")
#' # with an increase in sample size (n=1000), the values improve
#' small$n <- small$n * 10
#' mutual_local_expected(small, "race", "school", weight = "n")
#' }
#' @import data.table
#' @export
mutual_local_expected <- function(data, group, unit, weight = NULL,
                                  fixed_margins = TRUE, n_bootstrap = 100, base = exp(1)) {
    d <- prepare_data(data, group, unit, weight)
    n_total <- d[, sum(freq)]
    if (all.equal(n_total, round(n_total)) != TRUE) {
        stop(paste0(
            "bootstrap with a total sample size that is not an integer is not allowed, ",
            "maybe scale your weights?"
        ))
    }

    expected_compute(
        "ls", d, group, unit,
        fixed_margins, n_bootstrap, base
    )
}
