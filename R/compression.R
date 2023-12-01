#' Compresses a data matrix based on mutual information (segregation)
#'
#' Given a data set that identifies suitable neighbors for merging,
#' this function will merge units iteratively, where in each iteration
#' the neighbors with the smallest reduction in terms of total M will be merged.
#'
#' @param data A data frame.
#' @param group A categorical variable
#'   contained in \code{data}. Defines the first dimension
#'   over which segregation is computed.
#' @param unit A categorical variable
#'   contained in \code{data}. Defines the second dimension
#'   over which segregation is computed.
#' @param weight Numeric. Only frequency weights are allowed.
#'   (Default \code{NULL})
#' @param neighbors Either a data frame or a character. If data frame, then
#'   it needs exactly two columns, where each row identifies
#'   a set of "neighbors" that may be merged.
#'   If "local", considers the \code{n_neighbors} closest neighbors
#'   in terms of local segregation.
#'   If "all", all units are considered as possible neighbors. This
#'   may be very time-consuming.
#' @param n_neighbors Only relevant if \code{neighbors} is \code{"local"}.
#' @param max_iter Maximum number of iterations (Default \code{Inf})
#' @return Returns a data.table.
#' @import data.table
#' @export
compress <- function(data, group, unit, weight = NULL,
                     neighbors = "local", n_neighbors = 50, max_iter = Inf) {
    checkmate::assert_data_frame(data)
    checkmate::assert_vector(group, len = 1)
    checkmate::assert_vector(unit, len = 1)
    checkmate::assert_vector(weight, len = 1, null.ok = TRUE)
    if (is.data.frame(neighbors)) {
        checkmate::assert_data_frame(neighbors, ncols = 2)
    } else {
        checkmate::assert_choice(neighbors, c("all", "local"))
    }
    checkmate::assert_number(n_neighbors, lower = 1, finite = TRUE)
    checkmate::assert_number(max_iter, finite = FALSE)

    d <- prepare_data(data, group, unit, weight)

    if (!is.factor(d[[unit]]) && !is.character(d[[unit]])) {
        warning("coercing unit ids to character")
        d[[unit]] <- as.character(d[[unit]])
    }

    wide <- dcast(d, paste0(unit, "~", group), value.var = "freq", fill = 0)
    units <- as.character(wide[[unit]])
    wide[, (unit) := NULL]

    if (is.infinite(max_iter)) max_iter <- -1

    if (is.data.frame(neighbors)) {
        mat <- cbind(as.character(neighbors[[1]]), as.character(neighbors[[2]]))
        res <- compress_compute_cpp("df", mat, -1, as.matrix(wide), units, max_iter)
    } else if (neighbors == "all") {
        res <- compress_compute_cpp("all", matrix(""), -1, as.matrix(wide), units, max_iter)
    } else if (neighbors == "local") {
        res <- compress_compute_cpp("local", matrix(""), n_neighbors, as.matrix(wide), units, max_iter)
    }

    iterations <- as.data.table(res)

    initial_M <- mutual_total(d, group, unit, weight = "freq")[["est"]][1]
    iterations[, pct_M := M / initial_M]
    setnames(d, "freq", "n")

    compression <- list(
        iterations = iterations,
        initial_M = initial_M,
        unit = unit,
        group = group,
        data = d
    )
    class(compression) <- c("list", "segcompression")
    compression
}

#' @export
print.segcompression <- function(x, ...) {
    cat("Compression of dataset with ",
        x$data[, uniqueN(get(x$unit))],
        " units\n",
        "Original M: ",
        x$iterations$M[[1]],
        "; Final M: ",
        x$iterations$M[[nrow(x$iterations)]],
        "\n",
        sep = ""
    )
    for (t in c(0.99, 0.95, 0.9)) {
        row <- x$iterations[pct_M > t][.N]
        cat("- Threshold ", round(t * 100), "%: ",
            "M = ", row[["M"]],
            "; Units = ", row[["N_units"]],
            "\n",
            sep = ""
        )
    }
}

#' Scree plot for segregation compression
#'
#' A plot that allows to visually see the effect of compression
#' on mutual information.
#'
#' @param compression A "segcompression" object returned by \link{compress}.
#' @param tail Return only the last \code{tail} units (default: \code{Inf})
#' @return Returns a ggplot2 plot.
#' @import data.table
#' @export
scree_plot <- function(compression, tail = Inf) {
    if (!requireNamespace("ggplot2", quietly = TRUE)) {
        stop("Please install ggplot2 to use this function")
    }

    if (tail > nrow(compression$iterations)) tail <- Inf
    if (tail <= 0) tail <- Inf

    if (is.infinite(tail)) {
        to_plot <- compression$iterations[, .(N_units, M)]
        # add initial M
        initial <- data.table(
            N_units = to_plot[["N_units"]][1] + 1,
            M = compression$initial_M
        )
        to_plot <- rbindlist(list(initial, to_plot))
    } else {
        to_plot <- compression$iterations[(.N - tail + 1):.N, .(N_units, M)]
    }
    ggplot2::ggplot(
        to_plot,
        ggplot2::aes(x = N_units, y = M)
    ) +
        ggplot2::scale_y_continuous(
            sec.axis = ggplot2::sec_axis(~ . / compression$initial_M,
                name = "% of segregation information",
                labels = scales::percent_format(accuracy = 1)
            )
        ) +
        ggplot2::geom_point(alpha = .5) +
        ggplot2::scale_x_reverse() +
        ggplot2::ylab("M index") +
        ggplot2::theme_bw() +
        ggplot2::theme(legend.position = "none", panel.grid.minor = ggplot2::element_blank())
}

#' Creates a compressed dataset
#'
#' After running \link{compress}, this function creates a dataset where
#' units are merged.
#'
#' @param compression A "segcompression" object returned by \link{compress}.
#' @param n_units Determines the number of merges by specifying the number of
#'  units to remain in the compressed dataset.
#'  Only \code{n_units} or \code{percent} must be given. (default: \code{NULL})
#' @param percent Determines the number of merges by specifying the percentage
#'  of total segregation information retained in the compressed dataset.
#'  Only \code{n_units} or \code{percent} must be given. (default: \code{NULL})
#' @param parts (default: FALSE)
#' @return Returns a data.table.
#' @import data.table
#' @export
merge_units <- function(compression, n_units = NULL, percent = NULL, parts = FALSE) {
    cw <- get_crosswalk(compression, n_units, percent, parts = parts)
    data <- compression$data

    merged <- merge(data, cw, by = compression$unit, all.x = TRUE)
    if ("parts" %in% names(cw)) {
        merged <- merged[, .(n = sum(n), parts = first(parts)), by = c("new", compression$group)]
    } else {
        merged <- merged[, .(n = sum(n)), by = c("new", compression$group)]
    }
    setnames(merged, "new", compression$unit)
    merged
}

#' Create crosswalk after compression
#'
#' After running \link{compress}, this function creates a crosswalk table.
#' Usually it is preferred to call \link{merge_units} directly.
#'
#' @param compression A "segcompression" object returned by \link{compress}.
#' @param n_units Determines the number of merges by specifying the number of
#'  units to remain in the compressed dataset.
#'  Only \code{n_units} or \code{percent} must be given. (default: \code{NULL})
#' @param percent Determines the number of merges by specifying the percentage
#'  of total segregation information retained in the compressed dataset.
#'  Only \code{n_units} or \code{percent} must be given. (default: \code{NULL})
#' @param parts (default: FALSE)
#' @return Returns a ggplot2 plot.
#' @return Returns a data.table.
#' @export
get_crosswalk <- function(compression, n_units = NULL, percent = NULL, parts = FALSE) {
    if (is.null(n_units) && is.null(percent)) {
        stop("either n_units or percent has to be given")
    }
    if (!is.null(n_units) && !is.null(percent)) {
        stop("only n_units or percent has to be given")
    }
    if (!is.null(n_units)) {
        if (!(n_units %in% compression$iterations$N_units)) {
            stop("n_units is out of bounds")
        }
        n_iterations <- compression$iterations[N_units == n_units, iter]
    } else if (!is.null(percent)) {
        if (percent <= 0 || percent > 1) {
            stop("percent is out of bounds")
        }
        n_iterations <- compression$iterations[pct_M > percent][.N][["iter"]]
    }

    iterations <- compression$iterations[1:n_iterations, .(N_units, old_unit, new_unit)]

    bags <- get_crosswalk_cpp(iterations[, old_unit], iterations[, new_unit])

    merged <- data.table(
        unit = unlist(bags),
        new = paste0("M", rep(seq_len(length(bags)), lengths(bags)))
    )
    units <- unique(compression$data[[compression$unit]])
    unmerged_units <- units[!(units %in% unlist(bags))]
    unmerged <- data.table(unit = unmerged_units, new = unmerged_units)

    if (parts == TRUE) {
        merged[["parts"]] <- rep(
            sapply(bags, function(x) paste(sort(x), collapse = "/")),
            lengths(bags)
        )
    }

    combined <- rbindlist(list(merged, unmerged), fill = TRUE)
    setnames(combined, "unit", compression$unit)
    combined
}

#' @importFrom stats as.dendrogram
#' @export
as.dendrogram.segcompression <- function(object, ...) {
    if (utils::tail(object$iterations$N_units, 1) != 1) {
        stop("Compression algorithm needs to be run on complete dataset (max_iter = Inf)")
    }
    if (!requireNamespace("rrapply", quietly = TRUE)) {
        stop("Please install rrapply to use this function")
    }
    if (!requireNamespace("dendextend", quietly = TRUE)) {
        stop("Please install dendextend to use this function")
    }

    reduction <- 0
    tree <- list()

    for (i in seq_len(nrow(object$iterations))) {
        old_unit <- object$iterations[i, ][["old_unit"]]
        new_unit <- object$iterations[i, ][["new_unit"]]
        M_wgt <- object$iterations[i, ][["M_wgt"]]
        reduction <- reduction + M_wgt

        if (length(tree) > 0) {
            find_old <- rrapply::rrapply(tree,
                classes = "list",
                condition = function(x, .xname) x == old_unit,
                f = function(x, .xpos) .xpos,
                how = "flatten"
            )
            find_new <- rrapply::rrapply(tree,
                classes = "list",
                condition = function(x, .xname) x == new_unit,
                f = function(x, .xpos) .xpos,
                how = "flatten"
            )
        } else {
            find_old <- list()
            find_new <- list()
        }

        if (length(find_old) > 0 && length(find_new) > 0) {
            indices <- sort(c(find_old[[1]][1], find_new[[1]][1]))
            combined <- list(tree[[indices[1]]], tree[[indices[2]]])
            attr(combined, "height") <- reduction

            tree[[indices[1]]] <- combined
            tree[[indices[2]]] <- NULL
        } else if (length(find_old) > 0 || length(find_new) > 0) {
            if (length(find_old) > 0) {
                index <- find_old[[1]][1]
                new_item <- list(new_unit)
                attr(new_item, "label") <- new_unit
            } else {
                index <- find_new[[1]][1]
                new_item <- list(old_unit)
                attr(new_item, "label") <- old_unit
            }
            attr(new_item, "height") <- 0
            attr(new_item, "members") <- 1
            attr(new_item, "leaf") <- TRUE

            if (M_wgt == attr(tree[[index]], "height")) {
                tree[[index]][[length(tree[[index]]) + 1]] <- new_item
            } else {
                combined <- list(tree[[index]], new_item)
                attr(combined, "height") <- reduction
                tree[[index]] <- combined
            }
        } else {
            lhs <- list(old_unit)
            attr(lhs, "label") <- old_unit
            attr(lhs, "height") <- 0
            attr(lhs, "members") <- 1
            attr(lhs, "leaf") <- TRUE
            rhs <- list(new_unit)
            attr(rhs, "label") <- new_unit
            attr(rhs, "height") <- 0
            attr(rhs, "members") <- 1
            attr(rhs, "leaf") <- TRUE
            combined <- list(lhs, rhs)
            attr(combined, "height") <- M_wgt
            attr(combined, "members") <- 2
            tree[[length(tree) + 1]] <- combined
        }
    }

    dend <- tree[[1]]
    class(dend) <- "dendrogram"
    dend <- dendextend::fix_members_attr.dendrogram(dend)
    dend <- midcache.dendrogram(dend)
    return(dend)
}

# copied from stats -- not exported
midcache.dendrogram <- function(x) {
    stopifnot(inherits(x, "dendrogram"))
    setmid <- function(d, type) {
        depth <- 0L
        kk <- integer()
        jj <- integer()
        dd <- list()
        repeat {
            if (!stats::is.leaf(d)) {
                k <- length(d)
                depth <- depth + 1L
                kk[depth] <- k
                dd[[depth]] <- d
                d <- d[[jj[depth] <- 1L]]
                next
            }
            while (depth) {
                k <- kk[depth]
                j <- jj[depth]
                r <- dd[[depth]]
                r[[j]] <- unclass(d)
                if (j < k) {
                    break
                }
                depth <- depth - 1L
                midS <- sum(vapply(r, .midDend, 0))
                attr(r, "midpoint") <- (.memberDend(r[[1L]]) + midS) / 2
                d <- r
            }
            if (!depth) {
                break
            }
            dd[[depth]] <- r
            d <- r[[jj[depth] <- j + 1L]]
        }
        d
    }
    setmid(x)
}

.midDend <- function(x) {
    attr(x, "midpoint") %||% 0
}

.memberDend <- function(x) {
    attr(x, "x.member") %||% (attr(x, "members") %||% 1L)
}

`%||%` <- function(L, R) {
    if (is.null(L)) R else L
}
