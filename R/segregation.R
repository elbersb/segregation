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
#'  \item{}{\code{\link{usschools}} Example dataset.}
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
  "same_c_diff_m", "diff_c_same_m"
))

#' @import data.table
prepare_data <- function(data, unit, group, weight, expand, within = NULL) {
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

  # include variables and freq, and select only positive weights
  setDT(data)
  data <- data[freq > 0, c(vars, "freq"), with = FALSE]

  if (expand == TRUE) {
    # expanded df necessary for SE calculation
    data <- data[rep(1:.N, freq)]
    data[, "freq"] <- 1
  } else {
    # collapse, this speeds up calculation
    data <- data[, list(freq = sum(freq)), by = vars]
  }
  attr(data, "vars") <- vars
  data
}
