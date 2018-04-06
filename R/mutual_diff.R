#' Decomposes the difference between two M indices
#'
#' Uses either the method developed by Mora and Ruiz-Castillo (2009) or
#' the method developed by Elbers (2018)
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
#' @param method Either "mrc" (Mora and Ruiz-Castillo, 2009) or "elbers" (Elbers, 2018)
#' @param se If \code{TRUE}, standard errors are estimated via bootstrap.
#'   (Default \code{FALSE})
#' @param n_bootstrap Number of bootstrap iterations. (Default \code{50})
#'
#' @return Returns a list with the following elements:
#'   \code{M1} contains the M for \code{data1}.
#'   \code{M2} contains the M for \code{data2}.
#'   \code{diff} is the difference between \code{M2} and \code{M1}.
#'
#'   If \code{method} is "mrc", the list contains the following additional elements:
#'   \code{unit_entropy} is the difference in unit entropy.
#'   \code{group_marginal} is the contribution of group composition differences.
#'   \code{invariant} is the contribution of group composition-invariant differences.
#'   Note that \code{diff = unit_entropy + group_marginal + invariant}
#'
#'   If \code{method} is "elbers", the list contains the following additional elements:
#'   \code{group_marginal} is the contribution of the change in group marginals.
#'   \code{unit_marginal} is the contribution of the change in unit marginals.
#'   \code{conditional} is the contribution of the change in the conditional probabilities.
#'   \code{additions} is the weighted local linkage for \code{group} elements not in \code{data1}.
#'   \code{removals} is the weighted local linkage for \code{group} elements not in \code{data2}.
#'   Note that \code{diff = group_marginal + unit_marginal + conditional + additions - removals}
#'
#'   If \code{se} is \code{TRUE}, each element will be a vector of length two, with
#'   the second item containing the associated bootstrapped standard error.
#' @references
#' Ricardo Mora and Javier Ruiz-Castillo. 2009. "The Invariance Properties of the
#'   Mutual Information Index of Multigroup Segregation". Research on Economic Inequality 17: 33-53.
#'
#' Elbers, Benjamin. 2018. An Alternative Difference Decomposition of M. Working Paper.
#' @examples
#' @import data.table
#' @export
mutual_difference <- function(data1, data2, unit, group,
                            weight = NULL, method = NULL,
                            se = FALSE, n_bootstrap = 50) {
    if(method == "mrc") {
        method = mutual_difference_mrc_compute
    } else if(method == "elbers") {
        method = mutual_difference_elbers_compute
    } else {
        # raise error
    }

    d1 <- prepare_data(data1, unit, group, weight, expand = se)
    d2 <- prepare_data(data2, unit, group, weight, expand = se)

    if (se == FALSE) {
        ret <- method(d1, d2, unit, group)
    } else {
        vars <- attr(d1, "vars")
        boot_ret <- sapply(1:n_bootstrap, function(i) {
            cat(".")
            # resample and collapse by all variables, except 'freq'
            resampled1 <- d1[sample(.N, .N, replace = TRUE)][, list(freq = sum(freq)),
                                                             by = vars
                                                             ]
            resampled2 <- d2[sample(.N, .N, replace = TRUE)][, list(freq = sum(freq)),
                                                             by = vars
                                                             ]
            method(resampled1, resampled2, unit, group)
        })
        cat("\n")
        ret = list()
        for(i in 1:nrow(boot_ret)) {
            v = unlist(boot_ret[i,])
            ret[[i]] = c(mean(v), stats::sd(v))
        }
        names(ret) = dimnames(boot_ret)[[1]]
    }
    return(ret)
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

    list(M1 = M1, M2 = M2,
         diff = unit_entropy + group_marginal + cond,
         unit_entropy = unit_entropy,
         group_marginal = group_marginal,
         invariant = cond)
}

#' @import data.table
mutual_difference_elbers_compute <- function(d1, d2, unit, group) {
    n_total1 <- sum(d1$freq)
    n_total2 <- sum(d2$freq)

    d1[, `:=`(n_unit, sum(freq)), by = unit]
    d1[, `:=`(n_group, sum(freq)), by = group]
    d1[, `:=`(
        p_unit1 = n_unit / n_total1,
        p_group1 = n_group / n_total1,
        p_unit_g_group1 = freq / n_group
    )]
    d1[, `:=`(ll_part = p_unit_g_group1 * log(p_unit_g_group1 / p_unit1))]
    d1[, ls_group1 := sum(ll_part), by = group]
    M1 <- sum(d1[, list(p_group = first(p_group1),
                        ls_group = first(ls_group1)), by=group][, p_group*ls_group])

    d2[, `:=`(n_unit, sum(freq)), by = unit]
    d2[, `:=`(n_group, sum(freq)), by = group]
    d2[, `:=`(
        p_unit2 = n_unit / n_total2,
        p_group2 = n_group / n_total2,
        p_unit_g_group2 = freq / n_group
    )]
    d2[, `:=`(ll_part = p_unit_g_group2 * log(p_unit_g_group2 / p_unit2))]
    d2[, ls_group2 := sum(ll_part), by = group]
    M2 <- sum(d2[, list(p_group = first(p_group2),
                        ls_group = first(ls_group2)), by=group][, p_group*ls_group])

    setkeyv(d1, c(unit, group))
    setkeyv(d2, c(unit, group))

    joined <- merge(d1, d2, all=TRUE)
    # which groups are only in 1 or only in 2 & fill empty cells
    joined[, `:=`(
        miss1 = sum(is.na(p_group1)/.N),
        miss2 = sum(is.na(p_group2)/.N),
        ls_group1 = mean(ls_group1, na.rm=TRUE),
        ls_group2 = mean(ls_group2, na.rm=TRUE),
        p_group1 = mean(p_group1, na.rm=TRUE),
        p_group2 = mean(p_group2, na.rm=TRUE)), by=group]

    # split into three groups
    not_in_1 <- joined[miss1==1, list(p_group2=first(p_group2),
                                      ls_group2=first(ls_group2)), by=group]
    not_in_2 <- joined[miss2==1, list(p_group1=first(p_group1),
                                      ls_group1=first(ls_group1)), by=group]
    joined <- joined[miss1!=1 & miss2!=1]

    byg <- joined[, `:=`(
        same_c_diff_m = p_unit_g_group1 * log(p_unit_g_group1 / p_unit2),
        diff_c_same_m = p_unit_g_group2 * log(p_unit_g_group2 / p_unit1)
    )][, list(
        p_group1 = first(p_group1),
        p_group2 = first(p_group2),
        ls_group1 = first(ls_group1),
        ls_group2 = first(ls_group2),
        ls_marginal = 1/2*(sum(same_c_diff_m, na.rm=TRUE) - first(ls_group1)) +
            1/2*(first(ls_group2) - sum(diff_c_same_m, na.rm=TRUE)),
        ls_structural = 1/2*(sum(diff_c_same_m, na.rm=TRUE) - first(ls_group1)) +
            1/2*(first(ls_group2) - sum(same_c_diff_m, na.rm=TRUE))
    ), by = group]

    weight1 = byg$p_group2 %*% byg$ls_group2 - byg$p_group1 %*% byg$ls_group2
    weight2 = byg$p_group2 %*% byg$ls_group1 - byg$p_group1 %*% byg$ls_group1
    group_marginal = mean(c(weight1[1,1], weight2[1,1]))
    unit_marginal = (rowMeans(byg[, list(p_group1, p_group2)]) %*% byg$ls_marginal)[1,1]
    conditional = (rowMeans(byg[, list(p_group1, p_group2)]) %*% byg$ls_structural)[1,1]
    additions = (not_in_1$p_group2 %*% not_in_1$ls_group2)[1,1]
    removals = (not_in_2$p_group1 %*% not_in_2$ls_group1)[1,1]

    list(M1 = M1, M2 = M2,
         diff = group_marginal + unit_marginal + conditional + additions - removals,
         group_marginal = group_marginal,
         unit_marginal = unit_marginal,
         conditional = conditional,
         additions = additions,
         removals = removals
    )
}
