abs_diff <- function(x) {
    if (length(x) == 1) {
        abs(x)
    } else {
        abs(diff(x))
    }
}

#' @import data.table
dissimilarity_compute <- function(data, group, unit) {
    data[, n_group := sum(freq), by = group]
    est <- 1 / 2 * data[, abs_diff(freq / n_group), by = unit][, sum(V1)]
    data.table(stat = "D", est = est, stringsAsFactors = FALSE)
}

#' Calculates Index of Dissimilarity
#'
#' Returns the total segregation between \code{group} and \code{unit} using
#' the Index of Dissimilarity.
#'
#' @param data A data frame.
#' @param group A categorical variable or a vector of variables
#'   contained in \code{data}. Defines the first dimension
#'   over which segregation is computed. The D index only
#'   allows two distinct groups.
#' @param unit A categorical variable or a vector of variables
#'   contained in \code{data}. Defines the second dimension
#'   over which segregation is computed.
#' @param weight Numeric. (Default \code{NULL})
#' @param se If \code{TRUE}, the segregation estimates are bootstrapped to provide
#'   standard errors and to apply bias correction. The bias that is reported
#'   has already been applied to the estimates (i.e. the reported estimates are "debiased")
#'   (Default \code{FALSE})
#' @param CI If \code{se = TRUE}, compute the confidence (CI*100)% confidence interval
#'   in addition to the bootstrap standard error.
#'   This is based on percentiles of the bootstrap distribution, and a valid interpretation
#'   relies on a larger number of bootstrap iterations. (Default \code{0.95})
#' @param n_bootstrap Number of bootstrap iterations. (Default \code{100})
#' @return Returns a data.table with one row. The column \code{est} contains
#'   the Index of Dissimilarity.
#'   If \code{se} is set to \code{TRUE}, an additional column \code{se} contains
#'   the associated bootstrapped standard errors, an additional column \code{CI} contains
#'   the estimate confidence interval as a list column, an additional column \code{bias} contains
#'   the estimated bias, and the column \code{est} contains the bias-corrected estimates.
#' @references
#' Otis Dudley Duncan and Beverly Duncan. 1955. "A Methodological Analysis of Segregation Indexes,"
#'      American Sociological Review 20(2): 210-217.
#' @examples
#' # Example where D and H deviate
#' m1 <- matrix_to_long(matrix(c(100, 60, 40, 0, 0, 40, 60, 100), ncol = 2))
#' m2 <- matrix_to_long(matrix(c(80, 80, 20, 20, 20, 20, 80, 80), ncol = 2))
#' dissimilarity(m1, "group", "unit", weight = "n")
#' dissimilarity(m2, "group", "unit", weight = "n")
#' @import data.table
#' @export
dissimilarity <- function(data, group, unit, weight = NULL,
                          se = FALSE, CI = 0.95, n_bootstrap = 100) {
    stopifnot(CI > 0 & CI < 1)
    if (length(unique(data[[group]])) != 2) {
        stop("The D index only allows two distinct groups")
    }

    d <- prepare_data(data, group, unit, weight)
    ret <- dissimilarity_compute(d, group, unit)

    if (se == TRUE) {
        vars <- attr(d, "vars")
        n_total <- sum(d[["freq"]])

        if (all.equal(n_total, round(n_total)) == TRUE) {
            message(paste0(n_bootstrap, " bootstrap iterations on ", n_total, " observations"))
        } else {
            stop(paste0(
                "bootstrap with a total sample size that is not an integer is not allowed, ",
                "maybe scale your weights?"
            ))
        }
        # draw from a multinomial with weights specified by the cell counts
        draws <- stats::rmultinom(n_bootstrap, n_total, d[["freq"]] / n_total)

        boot_ret <- lapply(seq_len(n_bootstrap), function(i) {
            if (i %% 5 == 0) update_log(bs_n = i, bs_max = n_bootstrap)
            d[, freq := as.double(draws[, i])]
            dissimilarity_compute(d[freq > 0], group, unit)
        })
        close_log()
        boot_ret <- rbindlist(boot_ret)
        ret <- bootstrap_summary(ret, boot_ret, "stat", CI)
        setattr(ret, "bootstrap", boot_ret)
    }
    ret
}
