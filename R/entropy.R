#' Calculates the entropy of a distribution
#'
#' Returns the entropy of the distribution defined by
#' \code{group}.
#'
#' @param data A data frame.
#' @param group A categorical variable or a vector of variables
#'   contained in \code{data}.
#' @param weight Numeric. (Default \code{NULL})
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
entropy <- function(data, group, within = NULL, weight = NULL, base = exp(1)) {
    # use provided weight
    if (!is.null(weight)) {
        data[, "freq"] <- data[[weight]]
    } else {
        data[, "freq"] <- 1
    }
    data.table::setDT(data)
    n_total <- sum(data[, "freq"])

    if (is.null(within)) {
        vars <- group
        entropy_within <- 0
    } else {
        vars <- c(group, within)
        p_within <- data[, list(p = sum(freq)), by = within][["p"]] / n_total
        entropy_within <- -sum(p_within * logf(p_within, base))
    }

    p <- data[, list(p = sum(freq)), by = vars][["p"]] / n_total
    -sum(p * logf(p, base)) - entropy_within
}
