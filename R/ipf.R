#' Adjustment of marginal distributions using iterative proportional fitting
#'
#' Adjusts the marginal distributions for \code{group} and \code{unit}
#' in \code{source} to the respective marginal distributions in \code{target}, using the iterative
#' proportional fitting algorithm (IPF).
#'
#' The algorithm works by scaling
#' the marginal distribution of \code{group} in the \code{source} data frame towards the
#' marginal distribution of \code{target}; then repeating this process for \code{unit}. The
#' algorithm then keeps alternating between \code{group} and \code{unit} until the marginals
#' of the adjusted data frame are within the allowed precision. This results in a dataset that
#' retains the association structure of \code{source} while approximating
#' the marginal distribution of \code{target}. If the number of \code{unit} and
#' \code{group} categories is different in \code{source} and \code{target}, the data frame returns
#' the combination of \code{unit} and \code{group} categories that occur in both datasets.
#' Zero values are replaced by a small, non-zero number (1e-4).
#' Note that the values returned sum to the observations of the source data frame, not the
#' target data frame. This is different from other IPF implementations, but ensures that the IPF
#' does not change the number of observations.
#'
#' @param source A "source" data frame. The marginals of this
#'   dataset are adjusted to the marginals of \code{target}.
#' @param target A "target" data frame. The function returns a dataset
#'   where the marginal distributions of \code{group} and \code{unit} categories
#'   are approximated by those of \code{target}.
#' @param group A categorical variable or a vector of variables
#'   contained in \code{source} and \code{target}. Defines the first distribution
#'   for adjustment.
#' @param unit A categorical variable or a vector of variables
#'   contained in \code{source} and \code{target}. Defines the second distribution
#'   for adjustment.
#' @param weight Numeric. (Default \code{NULL})
#' @param max_iterations Maximum number of iterations used for the IPF algorithm.
#' @param precision Convergence criterion for the IPF algorithm. In every iteration,
#'   the ratio of the source and target marginals are calculated for every category of
#'   \code{group} and \code{unit}. The algorithm converges when all ratios are smaller
#'   than \code{1 + precision}.
#' @return Returns a data frame that retains
#'   the association structure of \code{source} while approximating
#'   the marginal distributions for \code{group} and \code{unit} of \code{target}.
#'   The dataset identifies each combination of \code{group} and \code{unit},
#'   and categories that only occur in either \code{source} or \code{target} are dropped.
#'   The adjusted frequency of each combination is given by the column \code{n},
#'   while \code{n_target} and \code{n_source} contain the zero-adjusted frequencies
#'   in the target and source dataset, respectively.
#' @references
#'   W. E. Deming and F. F. Stephan. 1940.
#'   "On a Least Squares Adjustment of a Sampled Frequency Table
#'   When the Expected Marginal Totals are Known".
#'   Annals of Mathematical Statistics. 11 (4): 427–444.
#'
#'  T. Karmel and M. Maclachlan. 1988.
#'   "Occupational Sex Segregation — Increasing or Decreasing?" Economic Record 64: 187-195.
#' @examples
#' # adjusts the marginals of group and unit categories so that
#' # schools00 has similar marginals as schools05
#' adj <- ipf(schools00, schools05, "race", "school", weight = "n")
#'
#' # check that the new "race" marginals are similar to the target marginals
#' # (the same could be done for schools)
#' aggregate(adj$n, list(adj$race), sum)
#' aggregate(adj$n_target, list(adj$race), sum)
#'
#' # note that the adjusted dataset contains fewer
#' # schools than either the source or the target dataset,
#' # because the marginals are only defined for the overlap
#' # of schools
#' length(unique(schools00$school))
#' length(unique(schools05$school))
#' length(unique(adj$school))
#' @import data.table
#' @export
ipf <- function(source, target, group, unit, weight = NULL,
                max_iterations = 100, precision = .001) {
    d1 <- prepare_data(source, group, unit, weight)
    d2 <- prepare_data(target, group, unit, weight)

    common_data <- create_common_data(d1, d2, group, unit)
    update_log(ipf_n = 1, ipf_max = 1)
    ret <- ipf_compute(common_data, group, unit, max_iterations, precision)
    close_log()

    as.data.frame(ret)
}

#' @import data.table
create_common_data <- function(d1, d2, group, unit, suppress_warnings = FALSE, fill_na = 1e-10) {
    # generate the crossproduct of common groups and units to
    # preserve all possible combinations
    common_group <- fintersect(d1[, group, with = FALSE], d2[, group, with = FALSE])
    common_unit <- fintersect(d1[, unit, with = FALSE], d2[, unit, with = FALSE])
    common_group$key <- 1
    common_unit$key <- 1
    common <- merge(common_unit, common_group, allow.cartesian = TRUE)
    common[, "key" := NULL]

    # this is optional because mutual_difference handles this internally -- no need
    # to print a warning here
    if (suppress_warnings == FALSE) {
        group_removed_d1 <- fsetdiff(d1[, group, with = FALSE], common_group[, group, with = FALSE])
        group_removed_d2 <- fsetdiff(d2[, group, with = FALSE], common_group[, group, with = FALSE])
        unit_removed_d1 <- fsetdiff(d1[, unit, with = FALSE], common_unit[, unit, with = FALSE])
        unit_removed_d2 <- fsetdiff(d2[, unit, with = FALSE], common_unit[, unit, with = FALSE])

        if (nrow(group_removed_d1) > 0) {
            warning(paste0(
                "IPF procedure removed ", nrow(group_removed_d1), " group categories from source, ",
                "this likely reduced the sample size (check columns n_source and n_target)"))
        }
        if (nrow(group_removed_d2) > 0) {
            warning(paste0(
                "IPF procedure removed ", nrow(group_removed_d2), " group categories from target, ",
                "this likely reduced the sample size (check columns n_source and n_target)"))
        }
        if (nrow(unit_removed_d1) > 0) {
            warning(paste0(
                "IPF procedure removed ", nrow(unit_removed_d1), " units from source, ",
                "this likely reduced the sample size (check columns n_source and n_target)"))
        }
        if (nrow(unit_removed_d1) > 0) {
            warning(paste0(
                "IPF procedure removed ", nrow(unit_removed_d2), " units from target, ",
                "this likely reduced the sample size (check columns n_source and n_target)"))
        }
    }

    # make sure that fill_na is much smaller than the smallest frequency
    min_freq <- min(c(d1[["freq"]], d2[["freq"]]))
    if (fill_na > min_freq) {
        fill_na <- min_freq / 1000
    }

    # join original frequencies
    common <- merge(common, d1, by = c(group, unit), all.x = TRUE)
    setnames(common, "freq", "freq_orig1")
    common <- merge(common, d2, by = c(group, unit), all.x = TRUE)
    setnames(common, "freq", "freq_orig2")
    # NA in freq1 and freq2 are replaced by a small non-zero number
    common[, freq1 := ifelse(is.na(freq_orig1), fill_na, freq_orig1)]
    common[, freq2 := ifelse(is.na(freq_orig2), fill_na, freq_orig2)]
    common[, freq_orig1 := ifelse(is.na(freq_orig1), 0, freq_orig1)]
    common[, freq_orig2 := ifelse(is.na(freq_orig2), 0, freq_orig2)]
    common
}


#' @import data.table
ipf_compute <- function(data, group, unit,
                        max_iterations = 100, precision = .001,
                        only_group = FALSE, only_unit = FALSE) {

    # work with relative weights
    data[, p1 := freq1 / sum(freq1)]
    data[, p2 := freq2 / sum(freq2)]
    data[, p_group_s := sum(p1), by = group]
    data[, p_unit_s := sum(p2), by = unit]

    if (only_group == TRUE) {
        data[, p_group_t := sum(p2), by = group]
        data[, p_unit_t := sum(p1), by = unit]
    } else if (only_unit == TRUE) {
        data[, p_group_t := sum(p1), by = group]
        data[, p_unit_t := sum(p2), by = unit]
    } else {
        data[, p_group_t := sum(p2), by = group]
        data[, p_unit_t := sum(p2), by = unit]
    }

    # IPF algorithm
    converged <- FALSE
    start_i <- sample(2, 1) # start randomly with either row or column adjustment
    max_iterations <- max_iterations - 1 + start_i # offset so max_iterations is still the same

    for (i in start_i:(max_iterations * 2)) {
        if (i %% 2 == 0) {
            data[, `:=`(p1 = p1 * p_unit_t / p_unit_s)]
        } else {
            data[, `:=`(p1 = p1 * p_group_t / p_group_s)]
        }
        data[, p1 := p1 / sum(p1)]

        if (i %% 5 == 0) {
            update_log_progress("#")
        }

        data[, p_group_s := sum(p1), by = group]
        data[, p_unit_s := sum(p1), by = unit]

        group_ratio <- data[, list(first(p_group_s), first(p_group_t)), by = group][, abs(V1 - V2)]
        unit_ratio <- data[, list(first(p_unit_s), first(p_unit_t)), by = unit][, abs(V1 - V2)]

        if (all(group_ratio <= precision) & all(unit_ratio <= precision)) {
            converged <- TRUE
            break
        }
    }

    if (!converged) {
        close_log()
        stop("IPF did not converge; lower precision or increase max_iterations.")
    }

    # adjust ipf-adjusted count to sample size of source
    data[, n := p1 / sum(p1) * sum(freq1)]
    setnames(data, "freq1", "n_source")
    setnames(data, "freq2", "n_target")

    data[, c("p_unit_t", "p_unit_s", "p_group_t", "p_group_s",
             "freq_orig1", "freq_orig2", "p1", "p2") := NULL]
    setkey(data, NULL)
    data
}
