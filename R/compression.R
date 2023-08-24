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
    stopifnot(length(group) == 1)
    stopifnot(length(unit) == 1)

    d <- prepare_data(data, group, unit, weight)

    if (is.factor(d[[unit]])) {
        d[[unit]] <- droplevels(d[[unit]])
    }
    if (is.factor(d[[group]])) {
        d[[group]] <- droplevels(d[[group]])
    }
    if (!is.factor(d[[unit]]) && !is.character(d[[unit]])) {
        warning("coercing unit ids to character")
        d[[unit]] <- as.character(d[[unit]])
    }

    if (is.data.frame(neighbors)) {
        stopifnot(ncol(neighbors) == 2)
    } else if (neighbors == "all") {
        all_units <- unique(d[[unit]])
        neighbors <- expand.grid(a = all_units, b = all_units)
    } else if (neighbors == "local") {
        ls <- mutual_local(d, group, unit, weight = "freq", wide = TRUE)
        entropy <- d[, .(entropy = entropy(.SD, group, weight = "freq")), by = unit]
        ls <- merge(ls, entropy)

        setorder(ls, entropy)
        neighbors <- lapply(2:(nrow(ls) - 1), function(u) {
            focal <- ls[[unit]][u]
            nb_before <- ls[[unit]][max(c(1, u - n_neighbors)):(u - 1)]
            nb_after <- ls[[unit]][(u + 1):min(c(nrow(ls), u + n_neighbors))]
            data.table(a = focal, b = c(nb_before, nb_after))
        })
        neighbors <- rbindlist(neighbors)
    } else {
        stop("neighbors: not a valid argument")
    }

    # rename -- easier
    setnames(d, unit, "unit")
    setnames(d, group, "group")
    setkeyv(d, "unit")

    # sort within rows to get rid of duplicates (i.e. 1-2 eq. 2-1)
    neighbors <- data.table::as.data.table(t(apply(neighbors, 1, sort)))
    neighbors <- unique(neighbors)[V1 != V2]

    # calculate M
    initial_M <- mutual_total(d, "group", "unit", weight = "freq")[["est"]][1]

    if (is.infinite(max_iter)) {
        total_units <- length(d[, unique(unit)])
        max_iter <- min(nrow(neighbors), total_units - 1)
    }

    m_neighbors <- as.matrix(neighbors)
    wide <- dcast(d, unit ~ group, value.var = "freq", fill = 0)
    m_data <- as.matrix(wide[, -"unit"])

    res <- compress_compute_cpp(m_neighbors, m_data, as.character(wide[, unit]), max_iter)
    iterations <- as.data.table(res)
    if (nrow(iterations) == 0) {
        stop("user interruption")
    }
    iterations[, pct_M := M / initial_M]

    setnames(d, "unit", unit)
    setnames(d, "group", group)
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

#' merge_units
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
    n <- iterations[, utils::tail(N_units, 1)]
    iterations[, N_units := NULL]

    bags <- list(c(iterations[1, old_unit], iterations[1, new_unit]))

    if (nrow(iterations) <= 1) {
        seq <- c()
    } else {
        seq <- 2:nrow(iterations)
    }

    for (i in seq) {
        old_unit <- iterations[i, old_unit]
        new_unit <- iterations[i, new_unit]

        old_unit_bag <- sapply(seq_len(length(bags)), function(ibag) old_unit %in% bags[[ibag]])
        new_unit_bag <- sapply(seq_len(length(bags)), function(ibag) new_unit %in% bags[[ibag]])

        if ((sum(old_unit_bag) + sum(new_unit_bag)) == 0) {
            bags[[length(bags) + 1]] <- c(old_unit, new_unit)
        } else if (sum(old_unit_bag) == 1 && sum(new_unit_bag) == 0) {
            bags[[which(old_unit_bag)]] <- c(bags[[which(old_unit_bag)]], new_unit)
        } else if (sum(old_unit_bag) == 0 && sum(new_unit_bag) == 1) {
            bags[[which(new_unit_bag)]] <- c(bags[[which(new_unit_bag)]], old_unit)
        } else if (sum(old_unit_bag) == 1 && sum(new_unit_bag) == 1) {
            bags[[length(bags) + 1]] <- c(
                bags[[which(new_unit_bag)]],
                bags[[which(old_unit_bag)]]
            )
            bags[[which(new_unit_bag)]] <- "X"
            bags[[which(old_unit_bag)]] <- "X"
            bags <- bags[bags != "X"]
        }
    }

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
