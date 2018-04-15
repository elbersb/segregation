#' segregation: Entropy-based segregation indices
#'
#' Computes entropy-based segregation indices, with a focus on
#' the mutual information index (M). The M is a measure
#' of segregation that is highly decomposable. Provides
#' tools to decompose the index by units and groups, and by within
#' and between terms. Includes standard error estimation by bootstrapping.
#'
#' @section Methods:
#'
#' \itemize{
#'  \item{}{\code{\link{mutual_total}} Computes M.}
#'  \item{}{\code{\link{mutual_local}} Computes local segregation based on M.}
#'  \item{}{\code{\link{mutual_difference}} Decomposes difference between two M indices.}
#' }
#' @section Data:
#'
#' \itemize{
#'  \item{}{\code{\link{schools00}} and \code{\link{schools05}} Example datasets.}
#' }
#'
#' @docType package
#' @name segregation
NULL

globalVariables(c(
    "freq",
    "n_group", "n_unit", "n_within", "n_within_unit",
    "p", "p_within", "p_group", "p_unit", "p_unit_g_group",
    "entropy_cond", "M_group", "ll_part",
    "cond1", "cond2", "entropy_cond1", "entropy_cond2",
    "group1", "group2", "p_group1",
    "p_group2", "p_unit_g_group1", "p_unit_g_group2",
    "p_unit1", "p_unit2", "sumcond1", "sumcond2",
    "miss1", "miss2",
    "ls_group", "ls_group1", "ls_group2",
    "same_c_diff_m", "diff_c_same_m", "est"
))

as_tibble_or_df <- function(data) {
    if (requireNamespace("tibble", quietly = TRUE)) {
        tibble::as_tibble(data)
    } else {
        df <- as.data.frame(data, stringsAsFactors = FALSE)
        rownames(df) = rownames(data)
        df
    }
}


#' @import data.table
prepare_data <- function(data, unit, group, weight, within = NULL) {
    vars <- c(unit, group)

    # use provided frequency weight
    if (!is.null(weight)) {
        data[, "freq"] <- data[, weight]
    } else {
        data[, "freq"] <- 1
    }

    # if within is not set, make up one that applies to the whole dataset
    if (!is.null(within)) {
        if (within == "within_dummy") {
            data[, within] <- 1
        }
        vars <- c(vars, within)
    }

    # collapse on vars, and select only positive weights
    setDT(data)
    data <- data[freq > 0, list(freq = sum(freq)), by = vars]
    attr(data, "vars") <- vars
    data
}
