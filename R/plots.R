#' A visual representation of segregation
#'
#' Produces a segregation plot.
#'
#' @param data A data frame.
#' @param group A categorical variable or a vector of variables
#'   contained in \code{data}. Defines the first dimension
#'   over which segregation is computed.
#' @param unit A categorical variable or a vector of variables
#'   contained in \code{data}. Defines the second dimension
#'   over which segregation is computed.
#' @param weight Numeric. (Default \code{NULL})
#' @param order A character, either
#'   "segregation", "entropy", "majority", or "majority_fixed".
#'   Affects the ordering of the units.
#'   The horizontal ordering of the groups can be changed
#'   by using a factor variable for \code{group}.
#'   The difference between "majority" and "majority_fixed" is that the former
#'   will reorder the groups in such a way that the majority group actually comes first.
#'   If you want to control the ordering yourself, use "majority_fixed" and specify
#'   the \code{group} variable as a factor variable.
#' @param reference_distribution Specifies the reference distribution, given as
#'   a two-column data frame, to be plotted on the right.
#'   If order is \code{segregation}, then this reference distribution is
#'   also used to compute the local segregation scores.
#' @param bar_space Specifies space between single units.
#' @param title Adds a plot title and appends the value of the H index.
#' @param axis_labels One of "left", "right", or "both". Determines where the
#'   y axis labels are placed.
#' @return Returns a ggplot2 object.
#' @import data.table
#' @export
segplot <- function(data, group, unit, weight, order = "segregation",
                    reference_distribution = NULL,
                    bar_space = 0, title = NULL, axis_labels = "left") {
    if (!requireNamespace("ggplot2", quietly = TRUE)) {
        stop("Please install ggplot2 to use this function")
    }

    stopifnot(length(group) == 1)
    stopifnot(length(unit) == 1)
    stopifnot(length(axis_labels) == 1)
    stopifnot(order %in% c("segregation", "entropy", "majority", "majority_fixed"))
    stopifnot(axis_labels %in% c("left", "right", "both"))

    d <- prepare_data(data, group, unit, weight)
    # easier if renamed
    setnames(d, group, "group")
    setnames(d, unit, "unit")

    d[, unit := as.character(unit)]

    d[, p := freq / sum(freq), by = .(unit)]
    d[, p_unit := sum(freq), by = .(unit)]
    N <- d[, first(p_unit), by = .(unit)][, sum(V1)]
    d[, p_unit := p_unit / N]

    # overall
    if (is.null(reference_distribution)) {
        overall <- d[, .(freq = sum(freq)), by = .(group)]
        overall[, p := freq / sum(freq)]
    } else {
        stopifnot(is.data.frame(reference_distribution))
        stopifnot(ncol(reference_distribution) == 2)
        stopifnot(names(reference_distribution) == c(group, "p"))
        stopifnot(nrow(reference_distribution) == d[, uniqueN(group)])
        overall <- as.data.table(reference_distribution)
        setnames(overall, group, "group")
    }
    # order by size
    setorder(overall, -p)
    group_order <- overall[["group"]]
    if (order == "majority") {
        # if majority, we always force a reordering by group size
        d[, group := as.character(group)]
        overall[, group := as.character(group)]
        d[, group := factor(group, levels = group_order)]
        overall[, group := factor(group, levels = group_order)]
    }

    wide <- data.table::dcast(d[, -"freq"], p_unit + unit ~ group, value.var = "p", fill = 0)

    if (order == "segregation") {
        ls <- merge(d, overall[, .(group, p_overall = p)], by = "group", all.x = TRUE)
        ls <- ls[, .(ls = sum(p * logf(p / p_overall))), by = .(unit)]
        wide <- merge(ls, wide, by = "unit")
        setorder(wide, -ls)
    } else if (order == "entropy") {
        ent <- d[, .(entropy = entropy(.SD, "group", weight = "freq")), by = .(unit)]
        wide <- merge(ent, wide, by = "unit")
        setorder(wide, entropy)
    } else if (order %in% c("majority", "majority_fixed")) {
        if (is.factor(d[["group"]])) {
            group_order <- d[, levels(group)]
        }
        setorderv(wide,
            c(group_order[[1]], utils::tail(group_order, 1)),
            order = c(1, -1)
        )
    }

    # format units
    wide[, xmin := cumsum(p_unit) - p_unit]
    wide[, xmax := cumsum(p_unit)]
    wide[, xmin := xmin + (.I - 1) * bar_space]
    wide[, xmax := xmax + (.I - 1) * bar_space]
    d <- merge(d, wide[, .(unit, xmin, xmax)], by = "unit")
    setorderv(d, c("xmin", "group"))
    d[, ymin := cumsum(p) - p, by = .(unit)]
    d[, ymax := cumsum(p), by = .(unit)]

    if (bar_space == 0) {
        breaks <- c(wide[["xmin"]], wide[, max(xmax)])
    } else {
        breaks <- c()
    }

    # format overall
    overall[, xmin := wide[, max(xmax)] + 0.1]
    overall[, xmax := wide[, max(xmax)] + 0.15]
    setorderv(overall, "group")
    overall[, ymin := cumsum(p) - p]
    overall[, ymax := cumsum(p)]

    combine <- rbindlist(list(d, overall), use.names = TRUE, fill = TRUE)
    plot <- ggplot2::ggplot(
        combine,
        ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)
    ) +
        ggplot2::geom_rect(ggplot2::aes(fill = .data[["group"]])) +
        ggplot2::scale_x_continuous(breaks = breaks, expand = c(0, 0)) +
        ggplot2::guides(fill = ggplot2::guide_legend(reverse = TRUE)) +
        ggplot2::theme_bw() +
        ggplot2::theme(
            panel.grid = ggplot2::element_blank(),
            axis.text.x = ggplot2::element_blank(),
            legend.position = "bottom"
        ) +
        ggplot2::labs(fill = NULL)

    if (axis_labels == "left") {
        plot <- plot + ggplot2::scale_y_continuous(
            labels = scales::percent_format(),
            expand = c(0, 0)
        )
    } else if (axis_labels == "right") {
        plot <- plot + ggplot2::scale_y_continuous(
            labels = scales::percent_format(),
            expand = c(0, 0),
            position = "right"
        )
    } else if (axis_labels == "both") {
        plot <- plot + ggplot2::scale_y_continuous(
            labels = scales::percent_format(),
            expand = c(0, 0),
            sec.axis = ggplot2::dup_axis()
        )
    }

    if (order == "segregation") {
        plot <- plot + ggplot2::labs(x = "< more segregated | less segregated >")
    }
    if (!is.null(title)) {
        H <- mutual_total(d, "group", "unit", weight = "freq")[stat == "H", est]
        plot <- plot + ggplot2::labs(title = paste0(title, " (H = ", round(H, 2), ")"))
    }
    plot
}


#' A visual representation of two-group segregation
#'
#' Produces a segregation curve, as defined in Duncan and Duncan (1955)
#'
#' @param data A data frame.
#' @param group A categorical variable or a vector of variables
#'   contained in \code{data}. Defines the first dimension
#'   over which segregation is computed.
#' @param unit A categorical variable or a vector of variables
#'   contained in \code{data}. Defines the second dimension
#'   over which segregation is computed.
#' @param weight Numeric. (Default \code{NULL})
#' @return Returns a ggplot2 object.
#' @import data.table
#' @export
segcurve <- function(data, group, unit, weight) {
    if (!requireNamespace("ggplot2", quietly = TRUE)) {
        stop("Please install ggplot2 to use this function")
    }

    stopifnot(length(group) == 1)
    stopifnot(length(unit) == 1)
    d <- prepare_data(data, group, unit, weight)
    # easier if renamed
    setnames(d, group, "group")
    setnames(d, unit, "unit")

    if (d[, uniqueN(group)] != 2) {
        stop("requires exactly two groups")
    }

    wide <- dcast(d, unit ~ group, value.var = "freq", fill = 0)
    group_names <- names(wide)[2:3]
    setnames(wide, group_names, c("group1", "group2"))
    wide[, pct_group_1 := group1 / (group1 + group2)]
    setorder(wide, pct_group_1)
    wide[, cumul_prob_1 := cumsum(group1) / sum(group1)]
    wide[, cumul_prob_2 := cumsum(group2) / sum(group2)]

    ggplot2::ggplot(wide, ggplot2::aes(x = cumul_prob_2, y = cumul_prob_1)) +
        ggplot2::annotate(geom = "segment", x = 0, y = 0, xend = 1, yend = 1, colour = "darkgray") +
        ggplot2::geom_line() +
        ggplot2::scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
        ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
        ggplot2::labs(
            x = paste("Cumulative % ", group_names[2]),
            y = paste("Cumulative % ", group_names[1])
        )
}
