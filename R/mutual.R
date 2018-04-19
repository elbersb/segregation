
#' @import data.table
mutual_total_compute <- function(data, group, unit, within) {
    # calculate totals
    n_total <- sum(data$freq)
    n_within <- data[, list(n_within_group = sum(freq)), by = c(within, group)]
    n_within[, `:=`(n_within = sum(n_within_group)), by = within]
    # calculate proportions and entropy for each within unit
    n_within[, `:=`(p_within = n_within / n_total, p = n_within_group / n_within)]
    entropy <- n_within[, list(entropy = sum(p * log(1 / p)), p_within = first(p_within)),
                        by = within
                        ]
    setkeyv(entropy, within)

    # calculate totals
    data[, `:=`(n_total, sum(freq)), by = within]
    data[, `:=`(n_unit, sum(freq)), by = c(within, unit)]
    data[, `:=`(p_unit = n_unit / n_total, p_group_g_unit = freq / n_unit)]

    # calculate entropy within groups
    grouped <- data[, list(p_unit = first(p_unit), entropy_cond = sum(p_group_g_unit *
                                                                            log(1 / p_group_g_unit))), by = c(within, unit)]
    setkeyv(grouped, within)
    # merge within entropy, and compare to group entropy
    grouped <- merge(grouped, entropy)
    by_within <- grouped[, list(
        part_m = sum(p_unit * (entropy - entropy_cond)),
        p_within = first(p_within)
    ), by = within]

    # min/max bounds
    categories <- data[, list(uniqueN(get(unit)), uniqueN(get(group))),]

    # total M is the sum of weighted within-group partial M
    data.table(stat = c("M", "M_min", "M_max"),
               est = c(sum(by_within$part_m %*% by_within$p_within),
                       0,
                       log(min(categories))))
}

#' Calculate the total mutual information index
#'
#' Returns the total segregation between \code{group} and \code{unit}.
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
#' @param weight Numeric. Only frequency weights are allowed.
#'   (Default \code{NULL})
#' @param se If \code{TRUE}, standard errors are estimated via bootstrap.
#'   (Default \code{FALSE})
#' @param n_bootstrap Number of bootstrap iterations. (Default \code{10})
#'
#' @return Returns a data frame with three rows. The column \code{est} contains
#'   in the first row total segregation. The second and third rows contain
#'   the lower and upper bounds of M, respectively.
#'   If \code{se} is set to \code{TRUE}, an additional column \code{se} contains
#'   the associated bootstrapped standard errors, and the column \code{est} contains
#'   bootstrapped estimates.
#' @references
#' Henri Theil. 1971. Principles of Econometrics. New York: Wiley.
#'
#' Ricardo Mora and Javier Ruiz-Castillo. 2011. "Entropy-based Segregation Indices". Sociological Methodology 41(1): 159–194.
#' @examples
#' # calculate school racial segregation
#' mutual_total(schools00, "school", "race", weight="n") # => .425
#'
#' # note that the definition of groups and units is arbitrary
#' mutual_total(schools00, "race", "school", weight="n") # => .425
#'
#' # if groups or units are defined by a combination of variables,
#' # vectors of variable names can be provided -
#' # here there is no difference, because schools
#' # are nested within districts
#' mutual_total(schools00, "race", c("district", "school"),
#'              weight="n") # => .424
#'
#' # estimate a standard error of M
#' mutual_total(schools00, "race", "school", weight="n", se=TRUE)
#'
#' # estimate segregation within school districts
#' mutual_total(schools00, "race", "school",
#'              within="district", weight="n") # => .087
#'
#' # estimate between-district racial segregation
#' mutual_total(schools00, "race", "district", weight="n") # => .338
#' # note that the sum of within-district and between-district
#' # segregation equals total school-race segregation;
#' # here, most segregation is between school districts
#' @import data.table
#' @export
mutual_total <- function(data, group, unit, within = NULL,
                         weight = NULL, se = FALSE, n_bootstrap = 10) {
    # define as a dummy variable that is equal for all cases
    if (is.null(within)) {
        within <- "within_dummy"
    }
    d <- prepare_data(data, group, unit, weight, within = within)

    if (se == FALSE) {
        ret <- mutual_total_compute(d, group, unit, within)
    } else {
        vars <- attr(d, "vars")
        n_total <- sum(d[, "freq"])
        boot_ret <- lapply(1:n_bootstrap, function(i) {
            cat(".")
            # resample and collapse by all variables, except "freq"
            resampled <- d[
                sample(.N, n_total, replace = TRUE, prob = freq)][,
                list(freq = .N), by = vars]
            mutual_total_compute(resampled, group, unit, within)
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
mutual_local_compute <- function(data, group, unit) {
    # generate unit and group totals
    n_total <- sum(data$freq)
    data[, `:=`(n_unit, sum(freq)), by = unit]
    data[, `:=`(n_group, sum(freq)), by = group]
    # generate unit and group proportions and the
    # conditional probability of being in any group given the unit
    data[, `:=`(
        p_unit = n_unit / n_total,
        p_group = n_group / n_total,
        p_group_g_unit = freq / n_unit
    )]
    # calculate local linkage, i.e. log(cond.) * log(cond./marginal)
    data[, `:=`(ll_part = p_group_g_unit * log(p_group_g_unit / p_group))]
    local <- data[, list(ls = sum(ll_part), p = first(p_unit)), by = unit]
    # calculate contribution to linkage linkage
    local[, `:=`(M_unit = ls * p)]
    # melt into long form
    melt(local,
         id.vars = unit, measure.vars = c("ls", "p", "M_unit"),
         variable.name = "stat", value.name = "est")
}

#' Calculates local segregation indices
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
#' @param weight Numeric. Only frequency weights are allowed.
#'   (Default \code{NULL})
#' @param se If \code{TRUE}, standard errors are estimated via bootstrap.
#'   (Default \code{FALSE})
#' @param n_bootstrap Number of bootstrap iterations. (Default \code{10})
#' @return Returns a data frame with three rows for each category defined by \code{unit},
#'   for a total of \code{3*(number of units)} rows. The column \code{est} defines three statistics that
#'   are provided for each unit: \code{ls}, the local segregation score,
#'   \code{p}, the proportion of the unit from the total number of cases, and
#'   \code{M_unit}, the product of \code{ls} and \code{p}.
#'   If \code{se} is set to \code{TRUE}, an additional column \code{se} contains
#'   the associated bootstrapped standard errors, and the column \code{est} contains
#'   bootstrapped estimates.
#' @references
#' Henri Theil. 1971. Principles of Econometrics. New York: Wiley.
#'
#' Ricardo Mora and Javier Ruiz-Castillo. 2011.
#'   "Entropy-based Segregation Indices". Sociological Methodology 41(1): 159–194.
#' @examples
#' # which racial groups are most segregated?
#' (localseg = mutual_local(schools00, "school", "race", weight="n"))
#' # native americans are most segregated, whites are least segregated.
#'
#' sum(localseg[localseg["stat"]=="p", "est"]) # => 1
#'
#' # the sum of the weighted local segregation scores equals
#' # total segregation
#' mutual_total(schools00, "school", "race", weight="n") # => .425
#' sum(localseg[localseg["stat"]=="M_unit", "est"]) # => .425
#' @import data.table
#' @export
mutual_local <- function(data, group, unit, weight = NULL, se = FALSE, n_bootstrap = 10) {
    d <- prepare_data(data, group, unit, weight)

    if (se == FALSE) {
        ret <- mutual_local_compute(d, group, unit)
    } else {
        vars <- attr(d, "vars")
        n_total <- sum(d[, "freq"])
        boot_ret <- lapply(1:n_bootstrap, function(i) {
            cat(".")
            # resample and collapse by all variables, except "freq"
            resampled <- d[
                sample(.N, n_total, replace = TRUE, prob = freq)][,
                list(freq = .N), by = vars]
            mutual_local_compute(resampled, group, unit)
        })
        cat("\n")
        boot_ret <- rbindlist(boot_ret)
        # summarize bootstrapped data frames
        ret <- boot_ret[, list(
            est = mean(est), se = stats::sd(est)),
            by = c(unit, "stat")]
    }
    # sort and return as data frame
    setorderv(ret, c("stat", unit))
    as_tibble_or_df(ret)
}
