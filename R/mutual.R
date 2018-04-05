
#' @import data.table
mutual_total_compute <- function(data, unit, group, within) {
  # calculate totals
  n_total <- sum(data$freq)
  n_within <- data[, list(n_within_unit = sum(freq)), by = c(within, unit)]
  n_within[, `:=`(n_within = sum(n_within_unit)), by = within]
  # calculate proportions and entropy for each within unit
  n_within[, `:=`(p_within = n_within / n_total, p = n_within_unit / n_within)]
  entropy <- n_within[, list(entropy = sum(p * log(1 / p)), p_within = first(p_within)),
                      by = within
                      ]
  setkeyv(entropy, within)

  # calculate totals
  data[, `:=`(n_total, sum(freq)), by = within]
  data[, `:=`(n_group, sum(freq)), by = c(within, group)]
  data[, `:=`(p_group = n_group / n_total, p_unit_g_group = freq / n_group)]

  # calculate entropy within groups
  grouped <- data[, list(p_group = first(p_group), entropy_cond = sum(p_unit_g_group *
                                                                        log(1 / p_unit_g_group))), by = c(within, group)]
  setkeyv(grouped, within)
  # merge within entropy, and compare to group entropy
  grouped <- merge(grouped, entropy)
  by_within <- grouped[, list(
    part_m = sum(p_group * (entropy - entropy_cond)),
    p_within = first(p_within)
  ), by = within]
  # total M is the sum of weighted within-group partial M
  sum(by_within$part_m %*% by_within$p_within)
}

#' Calculate the total mututal information index
#'
#' Returns the total segregation between \code{unit} and \code{group}.
#'
#' @param data A data frame.
#' @param unit A categorical variable or a vector of variables
#'   contained in \code{data}. Defines the first dimension
#'   over which segregation is computed.
#' @param group A categorical variable or a vector of variables
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
#' @return Returns a list with an element named \code{M}, containing total
#'   segregation. If \code{se} is set to \code{TRUE}, the list contains
#'   another element named \code{se} that contains the bootstrapped standard error.
#' @references
#' Henri Theil. 1971. Principles of Econometrics. New York: Wiley.
#'
#' Ricardo Mora and Javier Ruiz-Castillo. 2011. "Entropy-based Segregation Indices". Sociological Methodology 41(1): 159–194.
#' @examples
#' # calculate school racial segregation
#' mutual_total(usschools, 'school', 'race', weight='n') # => .422
#'
#' # note that the definition of units and groups is arbitrary
#' mutual_total(usschools, 'race', 'school', weight='n') # => .422
#'
#' # if units or groups are defined by a combination of variables,
#' # vectors of variable names can be provided -
#' # here there is no difference, because schools
#' # are nested within districts
#' mutual_total(usschools, 'race', c('district', 'school'),
#'              weight='n') # => .422
#'
#' # estimate a standard error of M
#' mutual_total(usschools, 'race', 'school', weight='n', se=TRUE)
#'
#' # estimate segregation within school districts
#' mutual_total(usschools, 'race', 'school',
#'              within='district', weight='n') # => .084
#'
#' # estimate between-district racial segregation
#' mutual_total(usschools, 'race', 'district', weight='n') # => .337
#' # note that the sum of within-district and between-district
#' # segregation equals total school-race segregation;
#' # here, most segregation is between school districts
#' @import data.table
#' @export
mutual_total <- function(data, unit, group, within = NULL,
                         weight = NULL, se = FALSE, n_bootstrap = 10) {
  # define as a dummy variable that is equal for all cases
  if (is.null(within)) {
    within <- "within_dummy"
  }
  d <- prepare_data(data, unit, group, weight, expand = se, within = within)

  if (se == FALSE) {
    M <- mutual_total_compute(d, unit, group, within)
    se <- NULL
  } else {
    # define vars (all except freq) for collapse
    vars <- names(d)[which(names(d) != "freq")]
    boot_M <- sapply(1:n_bootstrap, function(i) {
      cat(".")
      # resample and collapse by all variables, except 'freq'
      resampled <- d[sample(.N, .N, replace = TRUE)][, list(freq = sum(freq)),
                                                     by = vars
                                                     ]
      mutual_total_compute(resampled, unit, group, within)
    })
    cat("\n")
    se <- stats::sd(boot_M)
    M <- mean(boot_M)
  }

  # display and return
  ret <- list(M = M)
  di <- round(M, 4)
  if (!is.null(se)) {
    di <- paste0(di, " (", round(se, 4), ")")
    ret$se <- se
  }
  di <- paste0(
    di, " segegration of unit <", paste(unit, collapse = "+"), "> by group <",
    paste(group, collapse = "+"), ">"
  )
  if (within != "within_dummy") {
    di <- paste0(di, ", within <", paste(within, collapse = "+"), ">")
  }
  cat(paste0(di, "\n"))
  ret
}

#' @import data.table
mutual_local_compute <- function(data, unit, group) {
  # generate unit and group totals
  n_total <- sum(data$freq)
  data[, `:=`(n_unit, sum(freq)), by = unit]
  data[, `:=`(n_group, sum(freq)), by = group]
  # generate unit and group proportions and the
  # conditional probability of being in any unit given the group
  data[, `:=`(
    p_unit = n_unit / n_total,
    p_group = n_group / n_total,
    p_unit_g_group = freq / n_group
  )]
  # calculate local linkage, i.e. log(cond.) * log(cond./marginal)
  data[, `:=`(ll_part = p_unit_g_group * log(p_unit_g_group / p_unit))]
  local <- data[, list(ls = sum(ll_part), p = first(p_group)), by = group]
  # calculate contribution to linkage linkage
  local[, `:=`(M_group = ls * p)]
  local
}

#' Calculates local segregation indices
#'
#' Returns local segregation indices for each category defined
#' by \code{group}.
#'
#' @param data A data frame.
#' @param unit A categorical variable or a vector of variables
#'   contained in \code{data}. Defines the dimension
#'   over which segregation is computed.
#' @param group A categorical variable or a vector of variables
#'   contained in \code{data}. Defines the group for which local
#'   segregation indices are calculated.
#' @param weight Numeric. Only frequency weights are allowed.
#'   (Default \code{NULL})
#' @param se If \code{TRUE}, standard errors are estimated via bootstrap.
#'   (Default \code{FALSE})
#' @param n_bootstrap Number of bootstrap iterations. (Default \code{10})
#'
#' @return Returns a data frame, with one row for each of the categories
#'   defined by \code{group}. The first column(s) define the \code{group}.
#'   The column \code{ls} contains the local segregation index, with a
#'   column \code{ls_se} containing the standard error (if \code{se} is \code{TRUE}).
#'   The column \code{p} contains the proportion of each group from the total
#'   number of cases. The column \code{M_group} calculates \code{ls * p}. Standard
#'   errors for this column are contained in \code{M_group_se} if requested.
#' @references
#' Henri Theil. 1971. Principles of Econometrics. New York: Wiley.
#'
#' Ricardo Mora and Javier Ruiz-Castillo. 2011. "Entropy-based Segregation Indices". Sociological Methodology 41(1): 159–194.
#' @examples
#' # which racial groups are most segregated?
#' (localseg = mutual_local(usschools, 'school', 'race', weight='n'))
#' # native americans are most segregated, whites are least segregated.
#'
#' sum(localseg[, 'p']) # => 1
#'
#' # the sum of the weighted local segregation scores equals
#' # total segregation
#' mutual_total(usschools, 'school', 'race', weight='n') # => .422
#' sum(localseg[, 'M_group']) # => .422
#' @import data.table
#' @export
mutual_local <- function(data, unit, group, weight = NULL, se = FALSE, n_bootstrap = 10) {
  d <- prepare_data(data, unit, group, weight, expand = se)

  if (se == FALSE) {
    ls <- mutual_local_compute(d, unit, group)
    se <- NULL
  } else {
    # define vars (all except freq) for collapse
    vars <- names(d)[which(names(d) != "freq")]
    boot_ls <- lapply(1:n_bootstrap, function(i) {
      cat(".")
      # resample and collapse by all variables, except 'freq'
      resampled <- d[sample(.N, .N, replace = TRUE)][, list(freq = sum(freq)),
                                                     by = vars
                                                     ]
      mutual_local_compute(resampled, unit, group)
    })

    cat("\n")
    boot_ls <- rbindlist(boot_ls)
    # summarize bootstrapped data frames
    ls <- boot_ls[, list(
      ls = mean(ls), ls_se = stats::sd(ls),
      p = first(p), M_group = mean(M_group),
      M_group_se = stats::sd(M_group)
    ),
    by = group
    ]
  }

  # output total result
  M <- sum(ls$M_group)
  di <- paste0(
    round(M, 4), " segegration of unit <", paste(unit, collapse = "+"),
    "> by group <", paste(group, collapse = "+"), ">"
  )
  cat(paste0(di, "\n"))

  # sort and return as data frame
  setorderv(ls, group)
  as.data.frame(ls)
}
