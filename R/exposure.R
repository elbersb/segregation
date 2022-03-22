#' Calculates pairwise exposure
#'
#' Returns the pairwise exposure indices between groups
#'
#' @param data A data frame.
#' @param group A categorical variable
#'   contained in \code{data}. Defines the first dimension
#'   over which segregation is computed.
#' @param unit A vector of variables
#'   contained in \code{data}. Defines the second dimension
#'   over which segregation is computed.
#' @param weight Numeric. (Default \code{NULL})
#' @return Returns a data.table with columns "of", "to", and
#'  "exposure". Read results as "exposure of group x to group y".
#' @import data.table
#' @export
exposure <- function(data, group, unit, weight = NULL) {
    checkmate::assert_data_frame(data)
    checkmate::assert_character(group, len = 1)
    checkmate::assert_character(unit, min.len = 1)
    checkmate::assert_character(weight, null.ok = TRUE)
    checkmate::assert_names(names(data), must.include = c(group, unit, weight))

    of <- prepare_data(data, group, unit, weight)
    to <- copy(of)
    of[, n_unit := sum(freq), by = unit]
    of[, n_group := sum(freq), by = group]
    setnames(of, "freq", "freq_of")
    setnames(of, group, "of")
    setnames(to, "freq", "freq_to")
    setnames(to, group, "to")
    d <- merge(of, to, by = unit, allow.cartesian = TRUE)
    exp <- d[, .(exposure = sum(freq_of * freq_to / (n_unit * n_group))), by = .(of, to)]
    exp[]
}

#' Calculates isolation
#'
#' Returns isolation index of each group
#'
#' @param data A data frame.
#' @param group A categorical variable
#'   contained in \code{data}. Defines the first dimension
#'   over which segregation is computed.
#' @param unit A vector of variables
#'   contained in \code{data}. Defines the second dimension
#'   over which segregation is computed.
#' @param weight Numeric. (Default \code{NULL})
#' @return Returns a data.table with group column and isolation index.
#' @import data.table
#' @export
#' @import data.table
#' @export
isolation <- function(data, group, unit, weight = NULL) {
    checkmate::assert_data_frame(data)
    checkmate::assert_character(group, len = 1)
    checkmate::assert_character(unit, min.len = 1)
    checkmate::assert_character(weight, null.ok = TRUE)
    checkmate::assert_names(names(data), must.include = c(group, unit, weight))

    d <- prepare_data(data, group, unit, weight)

    d[, n_unit := sum(freq), by = unit]
    d[, n_group := sum(freq), by = group]
    iso <- d[, .(isolation = sum(freq^2 / (n_unit * n_group))), by = group]
    iso[]
}