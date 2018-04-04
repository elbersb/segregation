#' Ethnic/racial composition of primary schools in the U.S. for 2000/2001
#'
#' Subset of ethnic/racial composition data provided by
#' the National Center for Education Statistics, Common Core of Data,
#' with information on U.S. primary schools (with 100 students or more)
#' in Alabama, Colorado, and Conneticut.
#'
#' @format A data frame with 8,499 rows and 5 variables:
#' \describe{
#'   \item{state}{U.S. state}
#'   \item{district}{school agency/district ID}
#'   \item{school}{school ID}
#'   \item{race}{either native, asian, hispanic, black, or white}
#'   \item{n}{n of students by school and race}
#' }
#' @source \url{https://nces.ed.gov/ccd/}
"usschools"
