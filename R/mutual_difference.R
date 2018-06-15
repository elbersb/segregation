#' Decomposes the difference between two M indices
#'
#' Uses either a method based on the IPF algorithm (recommended and the default) or
#' the method developed by Mora and Ruiz-Castillo (2009).
#' 
#' The IPF method (Karmel and Maclachlan 1988) adjusts the margins of \code{data2} to be similar 
#' to the margins of \code{data1}. This is an iterative process, and may take of few seconds depending
#' on the size of the dataset (see \link{ipf} for details). 
#' The difference in M between \code{data1} and the margins-adjusted \code{data2} 
#' is the structural difference between \code{data1} and \code{data2}. 
#' The remaining, unexplained difference is due to changes in the marginal distribution.
#' Unless \code{forward_only} is set to \code{TRUE}, the process
#' is then repeated the other way around, and the differences are averaged.
#' 
#' A problem arises when there are \code{group} and/or \code{unit} categories in \code{data1}
#' that are not present in \code{data2} (or vice versa). The IPF method estimates the difference only
#' for categories that are present in both datasets, and reports additionally
#' the change in M that is induced by these cases as 
#' \code{additions} (present in \code{data2}, but not in \code{data1}) and 
#' \code{removals} (present in \code{data1}, but not in \code{data2}). For the method developed 
#' by Mora and Ruiz-Castillo (2009), there are two options provided: When using "mrc", the
#' categories not present in the other data source are set 0. When using "mrc_adjusted", the same 
#' procedure as for the IPF method is used, and \code{additions} and \code{removals} are reported.
#' 
#' Note that the IPF method is symmetric, i.e. the reversal of \code{group} and \code{unit}
#' definitions will yield the same results. The method developed by Mora and Ruiz-Castillo (2009)
#' is not symmetric, and will yield different results based on what is defined as the \code{group}
#' and \code{unit} categories.
#'
#' @param data1 A data frame with same structure as \code{data2}.
#' @param data2 A data frame with same structure as \code{data1}.
#' @param group A categorical variable or a vector of variables
#'   contained in \code{data}. Defines the first dimension
#'   over which segregation is computed.
#' @param unit A categorical variable or a vector of variables
#'   contained in \code{data}. Defines the second dimension
#'   over which segregation is computed.
#' @param weight Numeric. Only frequency weights are allowed.
#'   (Default \code{NULL})
#' @param method Either "ipf" (the default) (Karmel and Maclachlan 1988), or
#'   "mrc" / "mrc_adjusted" (Mora and Ruiz-Castillo 2009). See below for an explanation.
#' @param forward_only Only relevant for "ipf". If set to \code{TRUE}, the decomposition will
#'   only adjust the margins of \code{data2} to those \code{data1}, and not vice versa. This 
#'   is recommended when \code{data1} and \code{data2} are measurements at different points in time.
#'   (Default \code{FALSE})
#' @param se If \code{TRUE}, standard errors are estimated via bootstrap.
#'   (Default \code{FALSE})
#' @param n_bootstrap Number of bootstrap iterations. (Default \code{50})
#' @param ... Only used for additional arguments when
#'  when \code{method} is set to \code{ipf}. See \link{ipf} for details.
#' @param base Base of the logarithm that is used in the calculation.
#'   Defaults to the natural logarithm.
#' @return Returns a data frame with columns \code{stat} and \code{est}. The data frame contains
#'   the following rows defined by \code{stat}:
#'   \code{M1} contains the M for \code{data1}.
#'   \code{M2} contains the M for \code{data2}.
#'   \code{diff} is the difference between \code{M2} and \code{M1}.
#' 
#'   The sum of all rows following \code{diff} equal \code{diff}.
#' 
#'   When using "ipf" or "mrc_adjusted", two additional rows are reported:
#'   \code{additions} contains the change in M induces by \code{unit} and code{group} categories
#'   present in \code{data2} but not \code{data1}, and \code{removals} the reverse.
#'   
#'   When using "ipf", four additional rows are returned:
#'   \code{unit_marginal} is the contribution of unit composition differences.
#'   \code{group_marginal} is the contribution of group composition differences.
#'   \code{interaction} is the contribution of differences in the joint marginal distribution
#'      of \code{unit} and \code{group}. The total effect of changes in the margins is the sum
#'      of \code{unit_marginal}, \code{group_marginal}, and \code{interaction}.
#'   \code{structural} is the contribution unexplained by the marginal changes, i.e. the structural
#'     difference.
#'   
#'   When using "mrc" or "mrc_adjusted", three additional rows are returned:
#'   \code{unit_marginal} is the contribution of unit composition differences.
#'   \code{group_marginal} is the difference in group entropy.
#'   \code{structural} is the contribution of unit composition-invariant differences.
#'   For details on the interpretation of these terms, see Mora and Ruiz-Castillo (2009).
#' 
#'   If \code{se} is set to \code{TRUE}, an additional column \code{se} contains
#'   the associated bootstrapped standard errors, and the column \code{est} contains
#'   bootstrapped estimates.
#' @references
#' T. Karmel and M. Maclachlan. 1988.
#'   "Occupational Sex Segregation — Increasing or Decreasing?" Economic Record 64: 187-195.
#'
#' R. Mora and J. Ruiz-Castillo. 2009. "The Invariance Properties of the
#'   Mutual Information Index of Multigroup Segregation". Research on Economic Inequality 17: 33-53.
#' @examples
#' # decompose the difference in school segregation between 2000 and 2005
#' mutual_difference(schools00, schools05, group = "race", unit = "school",
#'     weight = "n", method = "ipf", precision = .01)
#' # => the structural component is close to zero, thus most change is in the marginals.
#' # note that this method gives identical results when we switch the unit and group definitions
#' mutual_difference(schools00, schools05, group = "school", unit = "race",
#'     weight = "n", method = "ipf", precision = .01)
#' 
#' # the MRC method indicates a much higher structural change
#' mutual_difference(schools00, schools05, group = "race", unit = "school",
#'     weight = "n", method = "mrc_adjusted")
#' # ...and is not symmetric
#' mutual_difference(schools00, schools05, group = "school", unit = "race",
#'     weight = "n", method = "mrc_adjusted")
#' @import data.table
#' @export
mutual_difference <- function(data1, data2, group, unit,
                              weight = NULL, method = "ipf", forward_only = FALSE,
                              se = FALSE, n_bootstrap = 50, base = exp(1), ...) {
    if(method == "ipf") {
        method <- function(...) { mutual_difference_ipf_compute(...) }
    } else if(method == "mrc") {
        method <- function(...) { mutual_difference_mrc_compute(adjusted = FALSE, ...) }
    } else if(method == "mrc_adjusted") {
        method <- function(...) { mutual_difference_mrc_compute(adjusted = TRUE, ...) }
    } else {
        stop("unknown decomposition method")
    }

    d1 <- prepare_data(data1, group, unit, weight)
    d2 <- prepare_data(data2, group, unit, weight)

    if (se == FALSE) {
        ret <- method(d1, d2, group, unit, forward_only, base, ...)
    } else {
        vars <- attr(d1, "vars")
        n_total1 <- sum(d1[, "freq"])
        n_total2 <- sum(d2[, "freq"])
        boot_ret <- lapply(1:n_bootstrap, function(i) {
            cat(".")
            # resample and collapse by all variables, except "freq"
            # coerce to double here, otherwise data.table complains later
            resampled1 <- d1[
                sample(.N, n_total1, replace = TRUE, prob = freq)][,
                list(freq = as.double(.N)), by = vars]
            resampled2 <- d2[
                sample(.N, n_total2, replace = TRUE, prob = freq)][,
                list(freq = as.double(.N)), by = vars]
            method(resampled1, resampled2, group, unit, forward_only, base, ...)
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


mutual_difference_ipf_compute <- function(d1, d2, group, unit, forward_only, base, ...) {
    m <- function(d, weight) {
        add_local(d, group, unit, base, weight)
        sum(d[, list(p = first(p_unit), ls = first(ls_unit)), by = unit][, p * ls])
    }

    M1 <- m(d1, "freq")
    M2 <- m(d2, "freq")
    d1[, c("p_unit", "p_group", "p_group_g_unit", 
        "n_unit", "n_group", "ls_unit") := NULL]
    d2[, c("p_unit", "p_group", "p_group_g_unit", 
        "n_unit", "n_group", "ls_unit") := NULL]

    d1_source <- create_common_data(d1, d2, group, unit)

    # compute M based on original counts (where zeros are not replaced)
    M1r <- m(d1_source, "freq_orig1")
    M2r <- m(d1_source, "freq_orig2")

    # compute for dataset 1, but with marginals from dataset 2
    d1_marg_d2 <- ipf_compute(d1_source, group, unit, ...)
    d1_marg_d2[, `:=`(n_group = sum(n_source), n_group_target = sum(n_target)), by = group]
    d1_marg_d2[, `:=`(n_unit = sum(n_source), n_unit_target = sum(n_target)), by = unit]
    d1_marg_d2[, `:=`(
            n_group_adj = n_source * n_group_target / n_group,
            n_unit_adj = n_source * n_unit_target / n_unit)]
    M_marg2 <- m(d1_marg_d2, "n")
    M_marg2_group <- m(d1_marg_d2, "n_group_adj")
    M_marg2_unit <- m(d1_marg_d2, "n_unit_adj")

    if(forward_only) {
        mix <- M_marg2 - M1r
        mix_group <- M_marg2_group - M1r
        mix_unit <- M_marg2_unit - M1r
        struct <- M2r - M_marg2
    } else {
        # both directions
        d2_source <- copy(d1_source)
        setnames(d2_source, c("freq1", "freq2"), c("freq2", "freq1"))

        # compute for dataset 2, but with marginals from dataset 1
        d2_marg_d1 <- ipf_compute(d2_source, group, unit, ...)
        d2_marg_d1[, `:=`(n_group = sum(n_source), n_group_target = sum(n_target)), by = group]
        d2_marg_d1[, `:=`(n_unit = sum(n_source), n_unit_target = sum(n_target)), by = unit]
        d2_marg_d1[, `:=`(
                n_group_adj = n_source * n_group_target / n_group,
                n_unit_adj = n_source * n_unit_target / n_unit)]
        M_marg1 <- m(d2_marg_d1, "n")
        M_marg1_group <- m(d2_marg_d1, "n_group_adj")
        M_marg1_unit <- m(d2_marg_d1, "n_unit_adj")

        mix <- 1/2 * (M_marg2 - M1r) + 1/2 * (M2r - M_marg1)
        mix_group <- 1/2 * (M_marg2_group - M1r) + 1/2 * (M2r - M_marg1_group)
        mix_unit <- 1/2 * (M_marg2_unit - M1r) + 1/2 * (M2r - M_marg1_unit)
        struct <- 1/2 * (M2r - M_marg2) + 1/2 * (M_marg1 - M1r)
    }

    stat = c("M1", "M2", "diff", "additions", "removals",
             "unit_marginal", "group_marginal", "interaction", "structural")
    est = c(M1, M2, M2 - M1, M2 - M2r, M1r - M1,
            mix_unit, mix_group, mix - mix_group - mix_unit, struct)
    data.table(stat = stat, est = est, stringsAsFactors = FALSE)
}


#' @import data.table
mutual_difference_mrc_compute <- function(d1, d2, group, unit, forward_only, base, adjusted) {
    m <- function(d) {
        sum(d[, list(p = first(p_unit), ls = first(ls_unit)), by = unit][, p * ls])
    }

    # first calculate M on the full set
    add_local(d1, group, unit, base)
    add_local(d2, group, unit, base)
    M1 <- m(d1)
    M2 <- m(d2)

    # reduce d1 and d2 to the joint set
    if(adjusted) {
        joined <- create_common_data(d1, d2, group, unit, fill_na = 0)

        # reduce M for d1
        add_local(joined, group, unit, base, "freq1")
        M1r <- m(joined)
        setnames(joined, c("p_unit", "p_group", "p_group_g_unit", "ls_unit"),
            c("p_unit1", "p_group1", "p_group_g_unit1", "ls_unit1"))
        # reduce M for d2
        add_local(joined, group, unit, base, "freq2")
        M2r <- m(joined)
        setnames(joined, c("p_unit", "p_group", "p_group_g_unit", "ls_unit"),
            c("p_unit2", "p_group2", "p_group_g_unit2", "ls_unit2"))
    } else {
        setnames(d1, c("freq", "p_unit", "p_group", "p_group_g_unit", "ls_unit"),
            c("freq1", "p_unit1", "p_group1", "p_group_g_unit1", "ls_unit1"))
        setnames(d2, c("freq", "p_unit", "p_group", "p_group_g_unit", "ls_unit"),
            c("freq2", "p_unit2", "p_group2", "p_group_g_unit2", "ls_unit2"))
        joined <- merge(d1, d2, by=c(group, unit), all = TRUE)
    }

    n_total1 <- sum(joined$freq1, na.rm = TRUE)
    n_total2 <- sum(joined$freq2, na.rm = TRUE)

    p_group1 <- joined[, list(p_group = sum(freq1, na.rm = TRUE) / n_total1),
                       by = group][, p_group]
    p_group2 <- joined[, list(p_group = sum(freq2, na.rm = TRUE) / n_total2),
                       by = group][, p_group]
    entropy_group1 <- sum(p_group1 * logf(1 / p_group1, base))
    entropy_group2 <- sum(p_group2 * logf(1 / p_group2, base))
    group_entropy <- entropy_group2 - entropy_group1

    joined <- joined[, list(
        sumcond1 = sum(p_group_g_unit1 * logf(p_group_g_unit1, base), na.rm = TRUE),
        sumcond2 = sum(p_group_g_unit2 * logf(p_group_g_unit2, base), na.rm = TRUE),
        entropy_cond1 = sum(p_group_g_unit1 * logf(1 / p_group_g_unit1, base), na.rm = TRUE),
        entropy_cond2 = sum(p_group_g_unit2 * logf(1 / p_group_g_unit2, base), na.rm = TRUE),
        p_unit1 = mean(p_unit1, na.rm = TRUE),
        p_unit2 = mean(p_unit2, na.rm = TRUE)), by = unit]
    joined[, p_unit1 := ifelse(is.na(p_unit1), 0, p_unit1)]
    joined[, p_unit2 := ifelse(is.na(p_unit2), 0, p_unit2)]
    joined <- joined[, list(
        cond1 = p_unit1 * sumcond2 - p_unit1 * sumcond1,
        cond2 = p_unit2 * sumcond2 - p_unit2 * sumcond1,
        unit1 = (p_unit2 - p_unit1) * sumcond2,
        unit2 = -(p_unit1 - p_unit2) * sumcond1,
        entropy_cond1 = entropy_cond1 * p_unit1,
        entropy_cond2 = entropy_cond2 * p_unit2)]
    cond = 1/2 * sum(joined[, cond1]) + 1/2 * sum(joined[, cond2])
    unit_marginal = 1/2 * sum(joined[, unit1]) + 1/2 * sum(joined[, unit2])

    if(adjusted) {
        additions <- M2 - M2r
        removals <- M1r - M1
        stat = c("M1", "M2", "diff", "additions", "removals",
            "unit_marginal", "group_marginal", "structural")
        est = c(M1, M2, M2 - M1, additions, removals,
            unit_marginal, group_entropy, cond)
    } else {
        stat = c("M1", "M2", "diff",
            "unit_marginal", "group_marginal", "structural")
        est = c(M1, M2, M2 - M1,
            unit_marginal, group_entropy, cond)
    }

    data.table(stat = stat, est = est, stringsAsFactors = FALSE)
}
