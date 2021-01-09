#' @import data.table
mutual_total_compute <- function(data, group, unit, base) {
    n_total <- sum(data[, freq])
    data[, n_unit := sum(freq), by = unit]
    data[, n_group := sum(freq), by = group]
    data[, `:=`(p = freq / n_total, p_exp = n_group * n_unit / (n_total^2))]

    # calculate M
    M <- data[p > 0, sum(p * log(p / p_exp, base = base))]

    # calculate H
    p <- data[, list(p = first(n_group / n_total)), by = group][["p"]]
    entropy_group <- sum(p * logf(1 / p, base))
    H <- M / entropy_group

    data.table(stat = c("M", "H"), est = c(M, H),
               stringsAsFactors = FALSE)
}

#' @import data.table
mutual_total_within_compute <- function(data, group, unit, within, base,
                                        components = FALSE) {
    # calculate totals
    n_total <- sum(data[, "freq"])
    n_within <- data[, list(n_within_group = sum(freq)), by = c(within, group)]
    n_within[, `:=`(n_within = sum(n_within_group)), by = within]
    # calculate proportions and entropy for each within unit
    n_within[, `:=`(p_within = n_within / n_total, p = n_within_group / n_within)]
    entropy <- n_within[, list(entropyw = sum(p * logf(1 / p, base)), p_within = first(p_within)),
                        by = within
                        ]
    setkeyv(entropy, within)

    # calculate totals
    data[, `:=`(n_total, sum(freq)), by = within]
    data[, `:=`(n_unit, sum(freq)), by = c(within, unit)]
    data[, `:=`(p_unit = n_unit / n_total, p_group_g_unit = freq / n_unit)]

    # calculate entropy within groups
    by_unit <- data[, list(
        p_unit = first(p_unit),
        entropy_cond = sum(p_group_g_unit * logf(1 / p_group_g_unit, base))),
        by = c(within, unit)]
    setkeyv(by_unit, within)

    # compute total entropy
    p <- data[, list(p = sum(freq)), by = group][["p"]] / n_total
    entropy_overall <- sum(p * logf(1 / p, base))

    # merge within entropy, and compare to group entropy
    by_unit <- merge(by_unit, entropy)
    by_within <- by_unit[, list(
        M = sum(p_unit * (entropyw - entropy_cond)),
        p = first(p_within),
        H = sum(p_unit * (entropyw - entropy_cond)) / first(entropyw),
        ent_ratio = first(entropyw) / entropy_overall
    ), by = within]
    by_within$H <- ifelse(is.finite(by_within$H), by_within$H, 0)

    if (components == TRUE) {
        melt(by_within,
             id.vars = within, measure.vars = c("M", "p", "H", "ent_ratio"),
             variable.name = "stat", value.name = "est",
             variable.factor = FALSE)
    } else {
        # total M is the sum of weighted within-group partial M
        M <- sum(by_within$M * by_within$p)
        H <- sum(by_within$H * by_within$p * by_within$ent_ratio)
        data.table(stat = c("M", "H"), est = c(M, H), stringsAsFactors = FALSE)
    }
}

#' Calculate total segregation for M and H
#'
#' Returns the total segregation between \code{group} and \code{unit}.
#' If \code{within} is given, calculates segregation within each
#' \code{within} category separately, and takes the weighted average.
#' Also see \code{\link{mutual_within}} for detailed within calculations.
#'
#' @param data A data frame.
#' @param group A categorical variable or a vector of variables
#'   contained in \code{data}. Defines the first dimension
#'   over which segregation is computed.
#' @param unit A categorical variable or a vector of variables
#'   contained in \code{data}. Defines the second dimension
#'   over which segregation is computed.
#' @param within A categorical variable or a vector of variables
#'   contained in \code{data}. The variable(s) should be a superset of either
#'   the \code{unit} or the \code{group} for the calculation to be meaningful.
#'   If provided, segregation is
#'   computed within the groups defined by the variable, and then averaged.
#'   (Default \code{NULL})
#' @param weight Numeric. (Default \code{NULL})
#' @param se If \code{TRUE}, the segregation estimates are bootstrapped to provide
#'   standard errors and to apply bias correction. The bias that is reported
#'   has already been applied to the estimates (i.e. the reported estimates are "debiased")
#'   (Default \code{FALSE})
#' @param CI If \code{se = TRUE}, compute the confidence (CI*100)% confidence interval
#'   in addition to the bootstrap standard error.
#'   This is based on percentiles of the bootstrap distribution, and a valid interpretation
#'   relies on a larger number of bootstrap iterations. (Default \code{0.95})
#' @param n_bootstrap Number of bootstrap iterations. (Default \code{100})
#' @param base Base of the logarithm that is used in the calculation.
#'   Defaults to the natural logarithm.
#' @return Returns a data.table with two rows. The column \code{est} contains
#'   the Mutual Information Index, M, and Theil's Entropy Index, H. The H is the
#'   the M divided by the \code{group} entropy. If \code{within} was given,
#'   M and H are weighted averages of the within-category segregation scores.
#'   If \code{se} is set to \code{TRUE}, an additional column \code{se} contains
#'   the associated bootstrapped standard errors, an additional column \code{CI} contains
#'   the estimate confidence interval as a list column, an additional column \code{bias} contains
#'   the estimated bias, and the column \code{est} contains the bias-corrected estimates.
#' @references
#' Henri Theil. 1971. Principles of Econometrics. New York: Wiley.
#'
#' Ricardo Mora and Javier Ruiz-Castillo. 2011.
#'      "Entropy-based Segregation Indices". Sociological Methodology 41(1): 159–194.
#' @examples
#' # calculate school racial segregation
#' mutual_total(schools00, "school", "race", weight = "n") # M => .425
#'
#' # note that the definition of groups and units is arbitrary
#' mutual_total(schools00, "race", "school", weight = "n") # M => .425
#'
#' # if groups or units are defined by a combination of variables,
#' # vectors of variable names can be provided -
#' # here there is no difference, because schools
#' # are nested within districts
#' mutual_total(schools00, "race", c("district", "school"),
#'              weight = "n") # M => .424
#'
#' # estimate standard errors and 95% CI for M and H
#' \dontrun{
#' mutual_total(schools00, "race", "school", weight = "n",
#'              se = TRUE, n_bootstrap = 1000)
#' }
#'
#' # estimate segregation within school districts
#' mutual_total(schools00, "race", "school",
#'              within = "district", weight = "n") # M => .087
#'
#' # estimate between-district racial segregation
#' mutual_total(schools00, "race", "district", weight = "n") # M => .338
#' # note that the sum of within-district and between-district
#' # segregation equals total school-race segregation;
#' # here, most segregation is between school districts
#' @import data.table
#' @export
mutual_total <- function(data, group, unit, within = NULL, weight = NULL,
                         se = FALSE, CI = 0.95, n_bootstrap = 100, base = exp(1)) {
    stopifnot(CI > 0 & CI < 1)
    d <- prepare_data(data, group, unit, weight, within)

    if (is.null(within)) {
        ret <- mutual_total_compute(d, group, unit, base)
    } else {
        ret <- mutual_total_within_compute(d, group, unit, within, base)
    }

    if (se == TRUE) {
        vars <- attr(d, "vars")
        n_total <- sum(d[["freq"]])

        if (all.equal(n_total, round(n_total)) == TRUE) {
            message(paste0(n_bootstrap, " bootstrap iterations on ", n_total, " observations"))
        } else {
            stop(paste0(
                "bootstrap with a total sample size that is not an integer is not allowed, ",
                "maybe scale your weights?"))
        }
        # draw from a multinomial with weights specified by the cell counts
        draws <- stats::rmultinom(n_bootstrap, n_total, d[["freq"]] / n_total)

        boot_ret <- lapply(seq_len(n_bootstrap), function(i) {
            if (i %% 5 == 0) update_log(bs_n = i, bs_max = n_bootstrap)
            d[, freq := as.double(draws[, i])]

            if (is.null(within)) {
                mutual_total_compute(d[freq > 0], group, unit, base)
            } else {
                mutual_total_within_compute(d[freq > 0], group, unit, within, base)
            }
        })
        close_log()
        boot_ret <- rbindlist(boot_ret)
        ret <- bootstrap_summary(ret, boot_ret, "stat", CI)
        setattr(ret, "bootstrap", boot_ret)
    }
    ret
}

#' Calculate detailed within-category segregation scores for M and H
#'
#' Calculates the segregation between \code{group} and \code{unit}
#' within each category defined by \code{within}.
#'
#' @param data A data frame.
#' @param group A categorical variable or a vector of variables
#'   contained in \code{data}. Defines the first dimension
#'   over which segregation is computed.
#' @param unit A categorical variable or a vector of variables
#'   contained in \code{data}. Defines the second dimension
#'   over which segregation is computed.
#' @param within A categorical variable or a vector of variables
#'   contained in \code{data} that defines the within-segregation categories.
#' @param weight Numeric. (Default \code{NULL})
#' @param se If \code{TRUE}, the segregation estimates are bootstrapped to provide
#'   standard errors and to apply bias correction. The bias that is reported
#'   has already been applied to the estimates (i.e. the reported estimates are "debiased")
#'   (Default \code{FALSE})
#' @param CI If \code{se = TRUE}, compute the confidence (CI*100)% confidence interval
#'   in addition to the bootstrap standard error.
#'   This is based on percentiles of the bootstrap distribution, and a valid interpretation
#'   relies on a larger number of bootstrap iterations. (Default \code{0.95})
#' @param n_bootstrap Number of bootstrap iterations. (Default \code{100})
#' @param base Base of the logarithm that is used in the calculation.
#'   Defaults to the natural logarithm.
#' @param wide Returns a wide dataframe instead of a long dataframe.
#'   (Default \code{FALSE})
#' @return Returns a data.table with four rows for each category defined by \code{within}.
#'   The column \code{est} contains four statistics that
#'   are provided for each unit:
#'   \code{M} is the within-category M, and \code{p} is the proportion of the category.
#'   Multiplying \code{M} and \code{p} gives the contribution of each within-category
#'   towards the total M.
#'   \code{H} is the within-category H, and \code{ent_ratio} provides the entropy ratio,
#'   defined as \code{EW/E}, where \code{EW} is the within-category entropy,
#'   and \code{E} is the overall entropy.
#'   Multiplying \code{H}, \code{p}, and \code{ent_ratio} gives the contribution of each within-category
#'   towards the total H.
#'   If \code{se} is set to \code{TRUE}, an additional column \code{se} contains
#'   the associated bootstrapped standard errors, an additional column \code{CI} contains
#'   the estimate confidence interval as a list column, an additional column \code{bias} contains
#'   the estimated bias, and the column \code{est} contains the bias-corrected estimates.
#'   If \code{wide} is set to \code{TRUE}, returns instead a wide dataframe, with one
#'   row for each \code{within} category, and the associated statistics in separate columns.
#' @references
#' Henri Theil. 1971. Principles of Econometrics. New York: Wiley.
#'
#' Ricardo Mora and Javier Ruiz-Castillo. 2011.
#'      "Entropy-based Segregation Indices". Sociological Methodology 41(1): 159–194.
#' @examples
#' (within <- mutual_within(schools00, "race", "school", within = "state",
#'                          weight = "n", wide = TRUE))
#' # the M for state "A" is .409
#' # manual calculation
#' schools_A <- schools00[schools00$state=="A",]
#' mutual_total(schools_A, "race", "school", weight = "n") # M => .409
#'
#' # to recover the within M and H from the output, multiply
#' # p * M and p * ent_ratio * H, respectively
#' sum(within$p * within$M) # => .326
#' sum(within$p * within$ent_ratio * within$H) # => .321
#' # compare with:
#' mutual_total(schools00, "race", "school", within = "state", weight = "n")
#' @import data.table
#' @export
mutual_within <- function(data, group, unit, within,
                         weight = NULL, se = FALSE, CI = 0.95, n_bootstrap = 100, base = exp(1),
                         wide = FALSE) {
    stopifnot(CI > 0 & CI < 1)
    d <- prepare_data(data, group, unit, weight, within)

    ret <- mutual_total_within_compute(d, group, unit, within, base, components = TRUE)

    if (se == TRUE) {
        vars <- attr(d, "vars")
        n_total <- sum(d[, "freq"])

        if (all.equal(n_total, round(n_total)) == TRUE) {
            message(paste0(n_bootstrap, " bootstrap iterations on ", n_total, " observations"))
        } else {
            stop(paste0(
                "bootstrap with a total sample size that is not an integer is not allowed, ",
                "maybe scale your weights?"))
        }

        # draw from a multinomial with weights specified by the cell counts
        draws <- stats::rmultinom(n_bootstrap, n_total, d[["freq"]] / n_total)

        boot_ret <- lapply(seq_len(n_bootstrap), function(i) {
            if (i %% 5 == 0) update_log(bs_n = i, bs_max = n_bootstrap)
            d[, freq := as.double(draws[, i])]
            mutual_total_within_compute(d[freq > 0], group, unit, within, base, components = TRUE)
        })
        close_log()
        boot_ret <- rbindlist(boot_ret)
        ret <- bootstrap_summary(ret, boot_ret, c(within, "stat"), CI)
        setattr(ret, "bootstrap", boot_ret)
    }

    if (wide == TRUE) {
        f <- stats::as.formula(paste(paste(within, collapse = "+"),
                                     "~ factor(stat, levels=c('M', 'p', 'H', 'ent_ratio'))"))
        if (se == TRUE) {
            ret <- dcast(ret, f, value.var = c("est", "se", "CI", "bias"))
            names(ret) <- c(within,
                            "M", "p", "H", "ent_ratio",
                            "M_se", "p_se", "H_se", "ent_ratio_se",
                            "M_CI", "p_CI", "H_CI", "ent_ratio_CI",
                            "M_bias", "p_bias", "H_bias", "ent_ratio_bias")
            setcolorder(ret, c(within,
                               "M", "M_se", "M_CI", "M_bias",
                               "p", "p_se", "p_CI", "p_bias",
                               "H", "H_se", "H_CI", "H_bias",
                               "ent_ratio", "ent_ratio_se", "ent_ratio_CI", "ent_ratio_bias"))
            setattr(ret, "bootstrap", boot_ret)
        } else {
            ret <- dcast(ret, f, value.var = c("est"))
        }
    }

    ret
}

#' @import data.table
mutual_local_compute <- function(data, group, unit, base = exp(1)) {
    add_local(data, group, unit, base)

    local <- data[, list(ls = first(ls_unit), p = first(p_unit)), by = unit]
    # melt into long form
    melt(local,
         id.vars = unit, measure.vars = c("ls", "p"),
         variable.name = "stat", value.name = "est",
         variable.factor = FALSE)
}

#' Calculates local segregation indices based on M
#'
#' Returns local segregation indices for each category defined
#' by \code{unit}.
#'
#' @param data A data frame.
#' @param group A categorical variable or a vector of variables
#'   contained in \code{data}. Defines the dimension
#'   over which segregation is computed.
#' @param unit A categorical variable or a vector of variables
#'   contained in \code{data}. Defines the group for which local
#'   segregation indices are calculated.
#' @param weight Numeric. (Default \code{NULL})
#' @param se If \code{TRUE}, the segregation estimates are bootstrapped to provide
#'   standard errors and to apply bias correction. The bias that is reported
#'   has already been applied to the estimates (i.e. the reported estimates are "debiased")
#'   (Default \code{FALSE})
#' @param CI If \code{se = TRUE}, compute the confidence (CI*100)% confidence interval
#'   in addition to the bootstrap standard error.
#'   This is based on percentiles of the bootstrap distribution, and a valid interpretation
#'   relies on a larger number of bootstrap iterations. (Default \code{0.95})
#' @param n_bootstrap Number of bootstrap iterations. (Default \code{100})
#' @param base Base of the logarithm that is used in the calculation.
#'   Defaults to the natural logarithm.
#' @param wide Returns a wide dataframe instead of a long dataframe.
#'   (Default \code{FALSE})
#' @return Returns a data.table with two rows for each category defined by \code{unit},
#'   for a total of \code{2*(number of units)} rows.
#'   The column \code{est} contains two statistics that
#'   are provided for each unit: \code{ls}, the local segregation score, and
#'   \code{p}, the proportion of the unit from the total number of cases.
#'   If \code{se} is set to \code{TRUE}, an additional column \code{se} contains
#'   the associated bootstrapped standard errors, an additional column \code{CI} contains
#'   the estimate confidence interval as a list column, an additional column \code{bias} contains
#'   the estimated bias, and the column \code{est} contains the bias-corrected estimates.
#'   If \code{wide} is set to \code{TRUE}, returns instead a wide dataframe, with one
#'   row for each \code{unit}, and the associated statistics in separate columns.
#' @references
#' Henri Theil. 1971. Principles of Econometrics. New York: Wiley.
#'
#' Ricardo Mora and Javier Ruiz-Castillo. 2011.
#'   "Entropy-based Segregation Indices". Sociological Methodology 41(1): 159–194.
#' @examples
#' # which schools are most segregated?
#' (localseg = mutual_local(schools00, "race", "school",
#'                          weight = "n", wide = TRUE))
#'
#' sum(localseg$p) # => 1
#'
#' # the sum of the weighted local segregation scores equals
#' # total segregation
#' sum(localseg$ls * localseg$p) # => .425
#' mutual_total(schools00, "school", "race", weight = "n") # M => .425
#' @import data.table
#' @export
mutual_local <- function(data, group, unit, weight = NULL,
                         se = FALSE, CI = 0.95, n_bootstrap = 100, base = exp(1),
                         wide = FALSE) {
    stopifnot(CI > 0 & CI < 1)
    d <- prepare_data(data, group, unit, weight)

    ret <- mutual_local_compute(d, group, unit, base)

    if (se == TRUE) {
        vars <- attr(d, "vars")
        n_total <- sum(d[, "freq"])

        if (all.equal(n_total, round(n_total)) == TRUE) {
            message(paste0(n_bootstrap, " bootstrap iterations on ", n_total, " observations"))
        } else {
            stop(paste0(
                "bootstrap with a total sample size that is not an integer is not allowed, ",
                "maybe scale your weights?"))
        }

        # draw from a multinomial with weights specified by the cell counts
        draws <- stats::rmultinom(n_bootstrap, n_total, d[["freq"]] / n_total)

        boot_ret <- lapply(seq_len(n_bootstrap), function(i) {
            if (i %% 5 == 0) update_log(bs_n = i, bs_max = n_bootstrap)
            d[, freq := as.double(draws[, i])]
            mutual_local_compute(d[freq > 0], group, unit, base)
        })
        close_log()
        boot_ret <- rbindlist(boot_ret)
        ret <- bootstrap_summary(ret, boot_ret, c(unit, "stat"), CI)
        setattr(ret, "bootstrap", boot_ret)
    }

    if (wide == TRUE) {
        f <- stats::as.formula(paste(paste(unit, collapse = "+"),
                                     "~ factor(stat, levels=c('ls', 'p'))"))
        if (se == TRUE) {
            ret <- dcast(ret, f, value.var = c("est", "se", "CI", "bias"))
            names(ret) <- c(unit, "ls", "p",
                "ls_se", "p_se",
                "ls_CI", "p_CI",
                "ls_bias", "p_bias")
            setcolorder(ret, c(unit,
                "ls", "ls_se", "ls_CI", "ls_bias",
                "p", "p_se", "p_CI", "p_bias"))
            setattr(ret, "bootstrap", boot_ret)
        } else {
            ret <- dcast(ret, f, value.var = c("est"))
        }
    }

    ret
}
