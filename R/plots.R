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
#' @param secondary_plot If \code{NULL} (default), no secondary plot is drawn.
#'   If "segregation", a secondary plot is drawn that shows adjusted local segregation
#'   scores for each unit. If "cumulative", a secondary plot is drawn that shows
#'   the cumulative contribution of each unit toward the total H (calculated as the
#'   proportion of each unit times the adjusted local segregation of each unit)0.
#' @param reference_distribution Specifies the reference distribution, given as
#'   a two-column data frame, to be plotted on the right.
#'   If order is \code{segregation}, then this reference distribution is
#'   also used to compute the local segregation scores.
#' @param bar_space Specifies space between single units.
#' @param hline Default \code{NULL}. If a color is specified,
#'   horizontal lines will be drawn where groups are separated.
#' @return Returns a ggplot2 or patchwork object.
#' @import data.table
#' @export
segplot <- function(data, group, unit, weight,
                    order = "segregation", secondary_plot = NULL,
                    reference_distribution = NULL,
                    bar_space = 0, hline = NULL) {
    if (!requireNamespace("patchwork", quietly = TRUE)) {
        stop("Please install patchwork to use this function")
    }
    if (!requireNamespace("ggplot2", quietly = TRUE)) {
        stop("Please install ggplot2 to use this function")
    }

    checkmate::assert_character(group, len = 1)
    checkmate::assert_character(unit, len = 1)
    d <- prepare_data(data, group, unit, weight)
    # easier if renamed
    setnames(d, group, "group")
    setnames(d, unit, "unit")

    # check other arguments
    checkmate::assert_choice(order, c("segregation", "entropy", "majority", "majority_fixed"))
    checkmate::assert_choice(secondary_plot, c("segregation", "cumulative"), null.ok = TRUE)
    checkmate::assert_data_frame(reference_distribution, ncols = 2, nrows = d[, uniqueN(group)], null.ok = TRUE)
    checkmate::assert_numeric(bar_space, len = 1, lower = 0)
    checkmate::assert_character(hline, len = 1, null.ok = TRUE)

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
        stopifnot(names(reference_distribution) == c(group, "p"))
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
    overall[, xmin := wide[, max(xmax)] + 0.05]
    overall[, xmax := wide[, max(xmax)] + 0.10]
    setorderv(overall, "group")
    overall[, ymin := cumsum(p) - p]
    overall[, ymax := cumsum(p)]

    combine <- rbindlist(list(d, overall), use.names = TRUE, fill = TRUE)
    plot <- ggplot2::ggplot(
        combine,
        ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)
    ) +
        ggplot2::geom_vline(xintercept = wide[, max(xmax)]) +
        ggplot2::geom_vline(xintercept = wide[, max(xmax) + 0.05]) +
        ggplot2::geom_rect(ggplot2::aes(fill = .data[["group"]])) +
        ggplot2::scale_x_continuous(breaks = breaks, expand = c(0, 0)) +
        ggplot2::scale_y_continuous(
            labels = scales::percent_format(),
            expand = c(0, 0),
            sec.axis = ggplot2::dup_axis()
        ) +
        ggplot2::guides(fill = ggplot2::guide_legend(reverse = TRUE)) +
        ggplot2::theme_bw() +
        ggplot2::theme(
            panel.grid = ggplot2::element_blank(),
            axis.text.x = ggplot2::element_blank(),
            legend.position = "right"
        ) +
        ggplot2::labs(fill = NULL)

    if (!is.null(hline)) {
        plot <- plot + ggplot2::geom_hline(yintercept = overall[1:(.N - 1), ymax], color = hline)
    }

    if (order == "segregation" && is.null(secondary_plot)) {
        plot <- plot + ggplot2::labs(x = "< more segregated | less segregated >")
    } else if (order == "segregation") {
        entropy <- entropy(overall, "group", weight = "p")
        H_index <- wide[, sum(ls * p_unit) / entropy]

        if (secondary_plot == "segregation") {
            wide[, stat := ls / entropy]
            label <- "adj. LS"
        } else if (secondary_plot == "cumulative") {
            wide[, stat := cumsum(p_unit * ls / entropy)]
            wide[, stat := max(stat) - stat + min(stat)]
            label <- "Cumulative"
        }

        stat_segments_h <- wide[, .(unit, x = ifelse(.I == 1, xmin, xmin - bar_space), xend = xmax, y = stat, yend = stat)]
        stat_segments_v <- wide[, .(unit, x = xmax, xend = xmax, y = stat, yend = shift(stat, type = "lead"))]
        stat_segments <- rbindlist(list(stat_segments_h, stat_segments_v))
        sub_plot <- ggplot2::ggplot(
            mapping = ggplot2::aes(x = x, y = y, xend = xend, yend = yend)
        ) +
            ggplot2::geom_vline(xintercept = wide[, max(xmax)], linewidth = 0.2) +
            ggplot2::geom_hline(yintercept = H_index, color = "orange") +
            ggplot2::geom_segment(data = stats::na.omit(stat_segments)) +
            ggplot2::theme_bw() +
            ggplot2::scale_x_continuous(
                limits = c(0, overall[, max(xmax)]), expand = c(0, 0),
                labels = function(x) scales::label_percent(1)(1 - x)
            ) +
            ggplot2::scale_y_continuous(sec.axis = ggplot2::dup_axis(
                breaks = H_index,
                labels = function(x) paste0("H = ", round(H_index, 3))
            )) +
            ggplot2::theme(
                panel.grid.major.x = ggplot2::element_blank(),
                panel.grid.minor.x = ggplot2::element_blank(),
                panel.grid.minor.y = ggplot2::element_blank(),
                axis.text.x = ggplot2::element_blank(),
                axis.ticks.x = ggplot2::element_blank(),
                axis.title.y.right = ggplot2::element_blank()
            ) +
            ggplot2::labs(y = label, x = "< more segregated | less segregated >")

        plot <- plot / sub_plot + patchwork::plot_layout(heights = c(4, 1))
    }

    plot
}


#' A visual representation of two-group segregation
#'
#' Produces one or several segregation curves, as defined in Duncan and Duncan (1955)
#'
#' @param data A data frame.
#' @param group A categorical variable contained in \code{data}.
#'   Defines the first dimension over which segregation is computed.
#' @param unit A categorical variable contained in \code{data}.
#'   Defines the second dimension over which segregation is computed.
#' @param weight Numeric. (Default \code{NULL})
#' @param segment A categorical variable contained in \code{data}. (Default \code{NULL})
#'   If given, several segregation curves will be shown, one for each segment.
#' @return Returns a ggplot2 object.
#' @import data.table
#' @export
segcurve <- function(data, group, unit, weight = NULL, segment = NULL) {
    if (!requireNamespace("ggplot2", quietly = TRUE)) {
        stop("Please install ggplot2 to use this function")
    }

    stopifnot(length(group) == 1)
    stopifnot(length(unit) == 1)
    d <- prepare_data(data, group, unit, weight, within = segment)
    # easier if renamed
    setnames(d, group, "group")
    setnames(d, unit, "unit")
    if (is.null(segment)) {
        d[["segment"]] <- 1
    } else {
        stopifnot(length(segment) == 1)
        setnames(d, segment, "segment")
        d[["segment"]] <- as.factor(d[["segment"]])
    }

    if (d[, uniqueN(group)] != 2) {
        stop("requires exactly two groups")
    }

    wide <- dcast(d, segment + unit ~ group, value.var = "freq", fill = 0)
    group_names <- names(wide)[3:4]
    setnames(wide, group_names, c("group1", "group2"))
    wide[, pct_group_1 := group1 / (group1 + group2)]
    setorder(wide, segment, pct_group_1)
    wide[, cumul_prob_1 := cumsum(group1) / sum(group1), by = .(segment)]
    wide[, cumul_prob_2 := cumsum(group2) / sum(group2), by = .(segment)]
    # need to add line through origin
    wide <- wide[, .(segment, cumul_prob_1, cumul_prob_2)]
    zeros <- wide[, .(cumul_prob_1 = 0, cumul_prob_2 = 0), by = .(segment)]
    wide <- rbindlist(list(wide, zeros))

    p <- ggplot2::ggplot(wide, ggplot2::aes(x = cumul_prob_2, y = cumul_prob_1)) +
        ggplot2::annotate(geom = "segment", x = 0, y = 0, xend = 1, yend = 1, colour = "darkgray") +
        ggplot2::scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
        ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
        ggplot2::labs(
            x = paste("Cumulative % ", group_names[2]),
            y = paste("Cumulative % ", group_names[1])
        ) +
        ggplot2::coord_fixed()

    if (is.null(segment)) {
        p <- p + ggplot2::geom_line()
    } else {
        p <- p +
            ggplot2::geom_line(ggplot2::aes(color = segment)) +
            ggplot2::labs(color = segment)
    }

    return(p)
}
