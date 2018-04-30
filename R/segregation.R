#' segregation: Entropy-based segregation indices
#'
#' Calculate and decompose entropy-based, multigroup segregation indices, with a focus
#' on the Mutual Information Index (M) and Theil's Information Index (H).
#' Provides tools to decompose the measures by groups and units, and by within
#' and between terms. Includes standard error estimation by bootstrapping.
#'
#' @section Methods:
#'
#' \itemize{
#'  \item{}{\code{\link{mutual_total}} Computes M and H.}
#'  \item{}{\code{\link{mutual_within}} Computes detailed within-category segregation.}
#'  \item{}{\code{\link{mutual_local}} Computes local segregation based on M.}
#'  \item{}{\code{\link{mutual_difference}} Decomposes difference between two M indices.}
#'  \item{}{\code{\link{entropy}} Calculates the entropy of a distribution.}
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
    "n_group", "n_unit", "n_within", "n_within_unit", "n_within_group",
    "p", "p_within", "p_group", "p_unit",
    "p_unit_g_group", "p_group_g_unit",
    "entropyw", "entropy_cond", "M_group", "ll_part",
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

logf <- function(v, base) {
    if(missing(base)) { stop("argument base required") }
    v <- v[v > 0]
    log(v, base=base)
}

#' Calculates the entropy of a distribution
#'
#' Returns the entropy of the distribution defined by
#' \code{group}.
#'
#' @param data A data frame.
#' @param group A categorical variable or a vector of variables
#'   contained in \code{data}.
#' @param weight Numeric. Only frequency weights are allowed.
#'   (Default \code{NULL})
#' @param base Base of the logarithm that is used in the entropy
#'   calculation. Defaults to the natural logarithm.
#' @return A single number, the entropy.
#' @examples
#' d <- data.frame(cat = c("A", "B"), n = c(25, 75))
#' entropy(d, "cat", weight = "n") # => .56
#' # this is equivalent to -.25*log(.25)-.75*log(.75)
#'
#' d <- data.frame(cat = c("A", "B"), n = c(50, 50))
#' # use base 2 for the logarithm, then entropy is maximized at 1
#' entropy(d, "cat", weight = "n", base = 2) # => 1
#' @import data.table
#' @export
entropy <- function(data, group, weight = NULL, base = exp(1)) {
    # use provided frequency weight
    if (!is.null(weight)) {
        data[, "freq"] <- data[, weight]
    } else {
        data[, "freq"] <- 1
    }
    setDT(data)
    n_total <- sum(data[, "freq"])
    p <- data[, list(p = sum(freq)), by = group][["p"]] / n_total
    sum(p * logf(1/p, base))
}


#' @import data.table
prepare_data <- function(data, group, unit, weight, within = NULL) {
    vars <- c(group, unit)

    # use provided frequency weight or weight of 1
    if (!is.null(weight)) {
        data[, "freq"] <- data[, weight]
    } else {
        data[, "freq"] <- 1
    }

    if (!is.null(within)) {
        vars <- c(vars, within)
    }

    # collapse on vars, and select only positive weights
    setDT(data)
    data <- data[freq > 0, list(freq = sum(freq)), by = vars]
    attr(data, "vars") <- vars
    data
}
