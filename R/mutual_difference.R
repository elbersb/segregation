#' Decomposes the difference between two M indices
#'
#' Uses one of three methods to decompose the difference between two M indices:
#' (1) "shapley" / "shapley_detailed": a method based on the Shapley decomposition
#' with a few advantages over the Karmel-Maclachlan method
#' (recommended and the default, Deutsch et al. 2006),
#' (2) "km": the method based on Karmel-Maclachlan (1988),
#' (3) "mrc": the method developed by Mora and Ruiz-Castillo (2009).
#' All methods have been extended to account for missing units/groups in either data input.
#'
#' The Shapley method is an improvement over the Karmel-Maclachlan method (Deutsch et al. 2006).
#' It is based on several margins-adjusted data inputs
#' and yields symmetrical results (i.e. \code{data1} and \code{data2} can be switched).
#' When "shapley_detailed" is used, the structural component is further decomposed into
#' the contributions of individuals units.
#'
#' The Karmel-Maclachlan method (Karmel and Maclachlan 1988) adjusts
#' the margins of \code{data1} to be similar to the margins of \code{data2}. This process
#' is not symmetrical.
#'
#' The Shapley and Karmel-Maclachlan methods are based on iterative proportional fitting (IPF),
#' first introduced by Deming and Stephan (1940).
#' Depending on the size of the dataset, this may take a few seconds (see \link{ipf} for details).
#'
#' The method developed by Mora and Ruiz-Castillo (2009) uses an algebraic approach to estimate the
#' size of the components. This will often yield substantively different results from the Shapley
#' and Karmel-Maclachlan methods. Note that this method is not symmetric in terms of what is
#' defined as \code{group} and \code{unit} categories, which may yield contradictory results.
#'
#' A problem arises when there are \code{group} and/or \code{unit} categories in \code{data1}
#' that are not present in \code{data2} (or vice versa).
#' All methods estimate the difference only
#' for categories that are present in both datasets, and report additionally
#' the change in M that is induced by these cases as
#' \code{additions} (present in \code{data2}, but not in \code{data1}) and
#' \code{removals} (present in \code{data1}, but not in \code{data2}).
#'
#'
#' @param data1 A data frame with same structure as \code{data2}.
#' @param data2 A data frame with same structure as \code{data1}.
#' @param group A categorical variable or a vector of variables
#'   contained in \code{data}. Defines the first dimension
#'   over which segregation is computed.
#' @param unit A categorical variable or a vector of variables
#'   contained in \code{data}. Defines the second dimension
#'   over which segregation is computed.
#' @param weight Numeric. (Default \code{NULL})
#' @param method Either "shapley" (the default), "km" (Karmel and Maclachlan method), or
#'   "mrc" (Mora and Ruiz-Castillo method).
#' @param se If \code{TRUE}, the segregation estimates are bootstrapped to provide
#'   standard errors and to apply bias correction. The bias that is reported
#'   has already been applied to the estimates (i.e. the reported estimates are "debiased")
#'   (Default \code{FALSE})
#' @param n_bootstrap Number of bootstrap iterations. (Default \code{100})
#' @param ... Only used for additional arguments when
#'  when \code{method} is set to \code{shapley} or \code{km}. See \link{ipf} for details.
#' @param base Base of the logarithm that is used in the calculation.
#'   Defaults to the natural logarithm.
#' @return Returns a data.table with columns \code{stat} and \code{est}. The data frame contains
#'   the following rows defined by \code{stat}:
#'   \code{M1} contains the M for \code{data1}.
#'   \code{M2} contains the M for \code{data2}.
#'   \code{diff} is the difference between \code{M2} and \code{M1}.
#'   The sum of the five rows following \code{diff} equal \code{diff}.
#'
#'   \code{additions} contains the change in M induces by \code{unit} and \code{group} categories
#'   present in \code{data2} but not \code{data1}, and \code{removals} the reverse.
#'
#'   All methods return the following three terms:
#'   \code{unit_marginal} is the contribution of unit composition differences.
#'   \code{group_marginal} is the contribution of group composition differences.
#'   \code{structural} is the contribution unexplained by the marginal changes, i.e. the structural
#'     difference. Note that the interpretation of these terms depend on the exact method used.
#'
#'   When using "km", one additional row is returned:
#'    \code{interaction} is the contribution of differences in the joint marginal distribution
#'      of \code{unit} and \code{group}.
#'
#'   When "shapley_detailed" is used, an additional column "unit" is returned, along with
#'     six additional rows for each unit that is present in both \code{data1} and \code{data2}.
#'     The five rows have the following meaning:
#'     \code{p1} (\code{p2}) is the proportion of the unit in \code{data1} (\code{data2})
#'     once non-intersecting units/groups have been removed. The changes in local linkage are
#'     given by \code{ls_diff1} and \code{ls_diff2}, and their average is given by
#'     \code{ls_diff_mean}. The row named \code{total}
#'     summarizes the contribution of
#'     the unit towards structural change
#'     using the formula \code{.5 * p1 * ls_diff1 + .5 * p2 * ls_diff2}.
#'     The sum of all "total" components equals structural change.
#'
#'   If \code{se} is set to \code{TRUE}, an additional column \code{se} contains
#'   the associated bootstrapped standard errors, an additional column \code{bias} contains
#'   the estimated bias, and the column \code{est} contains the bias-corrected estimates.
#' @references
#' W. E. Deming, F. F. Stephan. 1940. "On a Least Squares Adjustment of a Sampled Frequency Table
#'    When the Expected Marginal Totals are Known."
#'    The Annals of Mathematical Statistics 11(4): 427-444.
#'
#' T. Karmel and M. Maclachlan. 1988.
#'   "Occupational Sex Segregation — Increasing or Decreasing?" Economic Record 64: 187-195.
#'
#' R. Mora and J. Ruiz-Castillo. 2009. "The Invariance Properties of the
#'   Mutual Information Index of Multigroup Segregation." Research on Economic Inequality 17: 33-53.
#'
#' J. Deutsch, Y. Flückiger, and J. Silber. 2009.
#'       "Analyzing Changes in Occupational Segregation: The Case of Switzerland (1970–2000)."
#'        Research on Economic Inequality 17: 171–202.
#' @examples
#' # decompose the difference in school segregation between 2000 and 2005,
#' # using the Shapley method
#' mutual_difference(schools00, schools05, group = "race", unit = "school",
#'     weight = "n", method = "shapley", precision = .1)
#' # => the structural component is close to zero, thus most change is in the marginals.
#' # This method gives identical results when we switch the unit and group definitions,
#' # and when we switch the data inputs.
#'
#' # the Karmel-Maclachlan method is similar, but only adjust the data in the forward direction...
#' mutual_difference(schools00, schools05, group = "school", unit = "race",
#'     weight = "n", method = "km", precision = .1)
#'
#' # ...this means that the results won't be identical when we switch the data inputs
#' mutual_difference(schools05, schools00, group = "school", unit = "race",
#'     weight = "n", method = "km", precision = .1)
#'
#' # the MRC method indicates a much higher structural change...
#' mutual_difference(schools00, schools05, group = "race", unit = "school",
#'     weight = "n", method = "mrc")
#'
#' # ...and is not symmetric
#' mutual_difference(schools00, schools05, group = "school", unit = "race",
#'     weight = "n", method = "mrc")
#' @import data.table
#' @export
mutual_difference <- function(data1, data2, group, unit,
                              weight = NULL, method = "shapley",
                              se = FALSE, n_bootstrap = 100, base = exp(1), ...) {
    if (method == "shapley") {
        fun <- function(...) shapley_compute(..., detail = FALSE)
        cols <- "stat"
    } else if (method == "shapley_detailed") {
        fun <- function(...) shapley_compute(..., detail = TRUE)
        cols <- c("stat", unit)
    } else if (method == "km") {
        fun <- function(...) km_compute(...)
        cols <- "stat"
    } else if (method == "mrc") {
        fun <- function(...) mrc_compute(...)
        cols <- "stat"
    } else {
        stop("unknown decomposition method")
    }

    d1 <- prepare_data(data1, group, unit, weight)
    d2 <- prepare_data(data2, group, unit, weight)

    nrow_group <- nrow(merge(unique(d1[, group, with = FALSE]),
        unique(d2[, group, with = FALSE])))
    if (nrow_group == 0) stop("No overlap in group")
    nrow_unit <- nrow(merge(unique(d1[, unit, with = FALSE]),
        unique(d2[, unit, with = FALSE])))
    if (nrow_unit == 0) stop("No overlap in unit")

    ret <- fun(d1, d2, group, unit, base, ...)

    if (se == TRUE) {
        vars <- attr(d1, "vars")
        n_total1 <- sum(d1[, "freq"])
        n_total2 <- sum(d2[, "freq"])

        if (all.equal(n_total1, round(n_total1)) != TRUE |
            all.equal(n_total2, round(n_total2)) != TRUE) {
            stop(paste0(
                "bootstrap with a total sample size that is not an integer is not allowed, ",
                "maybe scale your weights?"))
        }

        # draw from a multinomial with weights specified by the cell counts
        draws1 <- stats::rmultinom(n_bootstrap, n_total1, d1[["freq"]] / n_total1)
        draws2 <- stats::rmultinom(n_bootstrap, n_total2, d2[["freq"]] / n_total2)

        boot_ret <- lapply(seq_len(n_bootstrap), function(i) {
            update_log(bs_n = i, bs_max = n_bootstrap)
            d1[, freq := as.double(draws1[, i])]
            d2[, freq := as.double(draws2[, i])]
            fun(d1[freq > 0], d2[freq > 0], group, unit, base, ...)
        })

        boot_ret <- rbindlist(boot_ret)
        ret_boot <- boot_ret[, list(
            mean_boot = mean(est), se = stats::sd(est)), by = cols]
        ret <- merge(ret, ret_boot, by = cols, sort = FALSE)
        # debias
        ret[, bias := mean_boot - est]
        ret[, est := est - bias]
        ret[, mean_boot := NULL]
        setattr(ret, "bootstrap", boot_ret)
    }
    close_log()
    data.table(ret)
}


shapley_compute <- function(d1, d2, group, unit, base, detail, ...) {
    m <- function(d, weight) {
        add_local(d, group, unit, base, weight)
        sum(d[, list(p = first(p_unit), ls = first(ls_unit)), by = unit][, p * ls])
    }

    # these are the original datasets containing ALL units and groups
    M1 <- m(d1, "freq")
    M2 <- m(d2, "freq")
    d1[, c("p_unit", "p_group", "p_group_g_unit",
        "n_unit", "n_group", "ls_unit") := NULL]
    d2[, c("p_unit", "p_group", "p_group_g_unit",
        "n_unit", "n_group", "ls_unit") := NULL]

    # group = A, unit = A, structure = A
    d_AAA <- create_common_data(d1, d2, group, unit, suppress_warnings = TRUE)
    # group = B, unit = B, structure = B
    d_BBB <- copy(d_AAA)
    setnames(d_BBB, c("freq1", "freq2"), c("freq2", "freq1"))
    setnames(d_BBB, c("freq_orig1", "freq_orig2"), c("freq_orig2", "freq_orig1"))

    update_log(ipf_n = 1, ipf_max = 6)
    d_BBA <- ipf_compute(copy(d_AAA), group, unit, ...)
    update_log(ipf_n = 2, ipf_max = 6)
    d_ABA <- ipf_compute(copy(d_AAA), group, unit, only_unit = TRUE, ...)
    update_log(ipf_n = 3, ipf_max = 6)
    d_BAA <- ipf_compute(copy(d_AAA), group, unit, only_group = TRUE, ...)

    update_log(ipf_n = 4, ipf_max = 6)
    d_AAB <- ipf_compute(copy(d_BBB), group, unit, ...)
    update_log(ipf_n = 5, ipf_max = 6)
    d_BAB <- ipf_compute(copy(d_BBB), group, unit, only_unit = TRUE, ...)
    update_log(ipf_n = 6, ipf_max = 6)
    d_ABB <- ipf_compute(copy(d_BBB), group, unit, only_group = TRUE, ...)

    # compute M based on unadjusted counts (where zeros are not replaced by small mumbers)
    # this is to ensure that M2 - m_BBB is exactly 0 when there are no additions
    m_AAA <- m(d_AAA, "freq_orig1")
    m_BBB <- m(d_BBB, "freq_orig1")
    m_BBA <- m(d_BBA, "n")
    m_ABA <- m(d_ABA, "n")
    m_BAA <- m(d_BAA, "n")
    m_AAB <- m(d_AAB, "n")
    m_BAB <- m(d_BAB, "n")
    m_ABB <- m(d_ABB, "n")

    marginal <- .5 * (m_BBA - m_AAA) + .5 * (m_BBB - m_AAB)
    structural <- .5 * (m_AAB - m_AAA) + .5 * (m_BBB - m_BBA)

    group_marginal <- .25 * (m_BAA - m_AAA + (m_BBA - m_ABA) +
                             m_BBB - m_ABB + (m_BAB - m_AAB))
    unit_marginal <- .25 * (m_ABA - m_AAA + (m_BBA - m_BAA) +
                            m_BBB - m_BAB + (m_ABB - m_AAB))
    stopifnot(round(group_marginal + unit_marginal, 4) == round(marginal, 4))
    stopifnot(round(M2 - m_BBB + m_AAA - M1 + marginal + structural, 4) == round(M2 - M1, 4))

    stat <- c("M1", "M2", "diff", "additions", "removals",
             "group_marginal", "unit_marginal", "structural")
    est <- c(M1, M2, M2 - M1, M2 - m_BBB, m_AAA - M1,
             group_marginal, unit_marginal, structural)

    ret <- data.table(stat = stat, est = est, stringsAsFactors = FALSE)
    if (detail == TRUE) {
        ls_AAB <- d_AAB[, list(ls_AAB = first(ls_unit)), by = unit]
        ls_AAA <- d_AAA[, list(p1 = first(p_unit), ls_AAA = first(ls_unit)), by = unit]
        ls_BBB <- d_BBB[, list(p2 = first(p_unit), ls_BBB = first(ls_unit)), by = unit]
        ls_BBA <- d_BBA[, list(ls_BBA = first(ls_unit)), by = unit]
        ls <- Reduce(merge, list(ls_AAB, ls_AAA, ls_BBB, ls_BBA))
        ls[, `:=`(ls_diff1 = ls_AAB - ls_AAA, ls_diff2 = ls_BBB - ls_BBA)]
        ls[, c("ls_AAB", "ls_AAA", "ls_BBB", "ls_BBA") := NULL]
        ls[, ls_diff_mean := .5 * ls_diff1 + .5 * ls_diff2]
        ls[, total := .5 * p1 * ls_diff1 + .5 * p2 * ls_diff2]
        ls <- melt(ls, unit, variable.name = "stat", value.name = "est")
        setorderv(ls, unit)
        ret <- rbindlist(list(ret, ls), fill = TRUE)
        cols <- c("stat", unit, "est")
        ret[, cols, with = FALSE]
    } else {
        ret
    }
}

km_compute <- function(d1, d2, group, unit, base, ...) {
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

    d1_source <- create_common_data(d1, d2, group, unit, suppress_warnings = TRUE)

    # compute M based on unadjusted counts (where zeros are not replaced by small mumbers)
    # this is to ensure that M2 - m_BBB is exactly 0 when there are no additions
    M1r <- m(d1_source, "freq_orig1")
    M2r <- m(d1_source, "freq_orig2")

    # compute for dataset 1, but with marginals from dataset 2
    update_log(ipf_n = 1, ipf_max = 1)
    d1_marg_d2 <- ipf_compute(copy(d1_source), group, unit, ...)
    d1_marg_d2[, `:=`(n_group = sum(n_source), n_group_target = sum(n_target)), by = group]
    d1_marg_d2[, `:=`(n_unit = sum(n_source), n_unit_target = sum(n_target)), by = unit]
    d1_marg_d2[, `:=`(
            n_group_adj = n_source * n_group_target / n_group,
            n_unit_adj = n_source * n_unit_target / n_unit)]
    M_marg2 <- m(d1_marg_d2, "n")
    M_marg2_group <- m(d1_marg_d2, "n_group_adj")
    M_marg2_unit <- m(d1_marg_d2, "n_unit_adj")

    mix <- M_marg2 - M1r
    mix_group <- M_marg2_group - M1r
    mix_unit <- M_marg2_unit - M1r
    struct <- M2r - M_marg2

    stat <- c("M1", "M2", "diff", "additions", "removals",
              "group_marginal", "unit_marginal", "interaction", "structural")
    est <- c(M1, M2, M2 - M1, M2 - M2r, M1r - M1,
             mix_group, mix_unit, mix - mix_group - mix_unit, struct)
    data.table(stat = stat, est = est, stringsAsFactors = FALSE)
}


#' @import data.table
mrc_compute <- function(d1, d2, group, unit, base) {
    m <- function(d) {
        sum(d[, list(p = first(p_unit), ls = first(ls_unit)), by = unit][, p * ls])
    }

    # first calculate M on the full set
    add_local(d1, group, unit, base)
    add_local(d2, group, unit, base)
    M1 <- m(d1)
    M2 <- m(d2)

    # reduce d1 and d2 to the joint set
    joined <- create_common_data(d1, d2, group, unit, suppress_warnings = TRUE, fill_na = 0)

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
        unit2 = - (p_unit1 - p_unit2) * sumcond1,
        entropy_cond1 = entropy_cond1 * p_unit1,
        entropy_cond2 = entropy_cond2 * p_unit2)]
    cond <- .5 * sum(joined[, cond1]) + .5 * sum(joined[, cond2])
    unit_marginal <- .5 * sum(joined[, unit1]) + .5 * sum(joined[, unit2])

    additions <- M2 - M2r
    removals <- M1r - M1
    stat <- c("M1", "M2", "diff", "additions", "removals",
        "unit_marginal", "group_marginal", "structural")
    est <- c(M1, M2, M2 - M1, additions, removals,
        unit_marginal, group_entropy, cond)

    data.table(stat = stat, est = est, stringsAsFactors = FALSE)
}
