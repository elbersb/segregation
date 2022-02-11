#' Ethnic/racial composition of schools for 2000/2001
#'
#' Fake dataset used for examples. Loosely based on data provided by
#' the National Center for Education Statistics, Common Core of Data,
#' with information on U.S. primary schools in three U.S. states.
#' The original data can be downloaded at \url{https://nces.ed.gov/ccd/}.
#'
#' @format A data frame with 8,142 rows and 5 variables:
#' \describe{
#'   \item{state}{either A, B, or C}
#'   \item{district}{school agency/district ID}
#'   \item{school}{school ID}
#'   \item{race}{either native, asian, hispanic, black, or white}
#'   \item{n}{n of students by school and race}
#' }
"schools00"

#' Ethnic/racial composition of schools for 2005/2006
#'
#' Fake dataset used for examples. Loosely based on data provided by
#' the National Center for Education Statistics, Common Core of Data,
#' with information on U.S. primary schools in three U.S. states.
#' The original data can be downloaded at \url{https://nces.ed.gov/ccd/}.
#'
#' @format A data frame with 8,013 rows and 5 variables:
#' \describe{
#'   \item{state}{either A, B, or C}
#'   \item{district}{school agency/district ID}
#'   \item{school}{school ID}
#'   \item{race}{either native, asian, hispanic, black, or white}
#'   \item{n}{n of students by school and race}
#' }
"schools05"

#' Student-level data including SES status
#'
#' Fake dataset used for examples. This is an individual-level
#' dataset of students in schools.
#'
#' @format A data frame with 5,153 rows and 3 variables:
#' \describe{
#'   \item{school_id}{school ID}
#'   \item{ethnic_group}{one of A, B, or C}
#'   \item{ses_quintile}{SES of the student (1 = lowest, 5 = highest)}
#' }
"school_ses"
