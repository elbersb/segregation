#' Decomposes the difference between two M indices
#'
#' Uses the method developed by Mora and Ruiz-Castillo (2009).
#'
#' @param data1 A data frame with same structure as \code{data2}.
#' @param data2 A data frame with same structure as \code{data1}.
#' @param unit A categorical variable or a vector of variables
#'   contained in \code{data}. Defines the first dimension
#'   over which segregation is computed.
#' @param group A categorical variable or a vector of variables
#'   contained in \code{data}. Defines the second dimension
#'   over which segregation is computed.
#' @param weight Numeric. Only frequency weights are allowed.
#'   (Default \code{NULL})
#' @param method for now, only "mrc" (Mora and Ruiz-Castillo, 2009) is possible
#' @param se If \code{TRUE}, standard errors are estimated via bootstrap.
#'   (Default \code{FALSE})
#' @param n_bootstrap Number of bootstrap iterations. (Default \code{50})
#'
#' @return Returns a data frame with columns \code{stat} and \code{est}. The data frame contains
#'   the following rows defined by \code{stat}:
#'   \code{M1} contains the M for \code{data1}.
#'   \code{M2} contains the M for \code{data2}.
#'   \code{diff} is the difference between \code{M2} and \code{M1}.
#'   \code{unit_entropy} is the difference in unit entropy.
#'   \code{group_marginal} is the contribution of group composition differences.
#'   \code{invariant} is the contribution of group composition-invariant differences.
#'   Note that \code{diff = unit_entropy + group_marginal + invariant}
#'
#'   If \code{se} is set to \code{TRUE}, an additional column \code{se} contains
#'   the associated bootstrapped standard errors, and the column \code{est} contains
#'   bootstrapped estimates.
#' @references
#' Ricardo Mora and Javier Ruiz-Castillo. 2009. "The Invariance Properties of the
#'   Mutual Information Index of Multigroup Segregation". Research on Economic Inequality 17: 33-53.
#' @examples
#' mutual_difference(schools00, schools05, unit="race", group="school",
#'     weight="n", method="mrc")
#' @import data.table
#' @export
mutual_difference <- function(data1, data2, unit, group,
                              weight = NULL, method = "mrc",
                              se = FALSE, n_bootstrap = 50) {
    if(method == "mrc") {
        method = mutual_difference_mrc_compute
    } else {
        stop("unknown method")
    }

    d1 <- prepare_data(data1, unit, group, weight)
    d2 <- prepare_data(data2, unit, group, weight)

    if (se == FALSE) {
        ret <- method(d1, d2, unit, group)
    } else {
        vars <- attr(d1, "vars")
        n_total1 <- sum(d1[, "freq"])
        n_total2 <- sum(d2[, "freq"])
        boot_ret <- lapply(1:n_bootstrap, function(i) {
            cat(".")
            # resample and collapse by all variables, except "freq"
            resampled1 <- d1[
                sample(.N, n_total1, replace = TRUE, prob = freq)][,
                list(freq = .N), by = vars]
            resampled2 <- d2[
                sample(.N, n_total2, replace = TRUE, prob = freq)][,
                list(freq = .N), by = vars]
            method(resampled1, resampled2, unit, group)
        })
        cat("\n")
        boot_ret <- rbindlist(boot_ret)
        # summarize bootstrapped data frames
        ret <- boot_ret[, list(
            est = mean(est), se = stats::sd(est)), by = c("stat")]
    }
    rownames(ret) <- ret[["stat"]]
    as_tibble_or_df(ret)
}

#' @import data.table
mutual_difference_mrc_compute <- function(d1, d2, unit, group) {
    n_total1 <- sum(d1$freq)
    n_total2 <- sum(d2$freq)

    p_unit1 <- d1[, list(p_unit = sum(freq) / n_total1), by = unit][, p_unit]
    p_unit2 <- d2[, list(p_unit = sum(freq) / n_total2), by = unit][, p_unit]
    entropy_unit1 <- sum(p_unit1 * log(1/p_unit1))
    entropy_unit2 <- sum(p_unit2 * log(1/p_unit2))
    unit_entropy <- entropy_unit2 - entropy_unit1

    d1[, `:=`(n_group = sum(freq)), by = group]
    d2[, `:=`(n_group = sum(freq)), by = group]
    d1[, `:=`(
        p_group1 = n_group / n_total1,
        p_unit_g_group1 = freq / n_group
    )]
    d2[, `:=`(
        p_group2 = n_group / n_total2,
        p_unit_g_group2 = freq / n_group
    )]
    setkeyv(d1, c(unit, group))
    setkeyv(d2, c(unit, group))

    joined <- merge(d1, d2, all=TRUE)[, list(
        sumcond1 = sum(p_unit_g_group1 * log(p_unit_g_group1), na.rm = TRUE),
        sumcond2 = sum(p_unit_g_group2 * log(p_unit_g_group2), na.rm = TRUE),
        entropy_cond1 = sum(p_unit_g_group1 * log(1 / p_unit_g_group1), na.rm = TRUE),
        entropy_cond2 = sum(p_unit_g_group2 * log(1 / p_unit_g_group2), na.rm = TRUE),
        p_group1 = mean(p_group1, na.rm = TRUE),
        p_group2 = mean(p_group2, na.rm = TRUE)), by=group]
    joined[, p_group1:=ifelse(is.na(p_group1), 0, p_group1)]
    joined[, p_group2:=ifelse(is.na(p_group2), 0, p_group2)]
    joined <- joined[, list(
        cond1 = p_group1 * sumcond2 - p_group1 * sumcond1,
        cond2 = p_group2 * sumcond2 - p_group2 * sumcond1,
        group1 = (p_group2 - p_group1) * sumcond2,
        group2 = -(p_group1 - p_group2) * sumcond1,
        entropy_cond1 = entropy_cond1 * p_group1,
        entropy_cond2 = entropy_cond2 * p_group2)]
    cond = 1/2*sum(joined[, cond1]) + 1/2*sum(joined[, cond2])
    group_marginal = 1/2*sum(joined[, group1]) + 1/2*sum(joined[, group2])
    M1 <- entropy_unit1 - sum(joined[, entropy_cond1])
    M2 <- entropy_unit2 - sum(joined[, entropy_cond2])

    stat = c("M1", "M2", "diff",
             "group_marginal", "unit_entropy", "invariant")
    est = c(M1, M2, unit_entropy + group_marginal + cond,
            group_marginal, unit_entropy, cond)
    data.table(stat = stat, est = est)
}

