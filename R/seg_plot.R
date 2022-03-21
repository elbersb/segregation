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
#'   "segregation", "entropy", or "majority".
#'   Affects the ordering of the units.
#' @param reference_distribution Specifies the reference distribution, given as
#'   a two-column data frame, to be plotted on the right.
#'   If order is \code{segregation}, then this reference distribution is
#'   also used to compute the local segregation scores.
#' @param bar_space Specifies space between single units.
#' @return Returns a ggplot2 object.
#' @import data.table
#' @export
seg_plot <- function(data, group, unit, weight, order = "segregation",
                     reference_distribution = NULL,
                     bar_space = 0) {
    if (!requireNamespace("ggplot2", quietly = TRUE)) {
        stop("Please install ggplot2 to use this function")
    }

    stopifnot(length(group) == 1)
    stopifnot(length(unit) == 1)
    stopifnot(order %in% c("segregation", "entropy", "majority"))
    d <- prepare_data(data, group, unit, weight)
    # easier if renamed
    setnames(d, group, "group")
    setnames(d, unit, "unit")

    d[, group := as.character(group)]
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
        overall[, group := as.character(group)]
    }
    setorder(overall, -p)
    group_order <- overall[["group"]]

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
    } else if (order == "majority") {
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
    d[, group := factor(group, levels = group_order)]
    setorderv(d, c("xmin", "group"))
    d[, ymin := cumsum(p) - p, by = .(unit)]
    d[, ymax := cumsum(p), by = .(unit)]

    if (bar_space == 0) {
        breaks <- c(wide[["xmin"]], wide[, max(xmax)])
    } else {
        breaks <- c()
    }

    # format overall
    overall[, group := factor(group, levels = group_order)]
    overall[, ymin := cumsum(p) - p]
    overall[, ymax := cumsum(p)]
    overall[, xmin := wide[, max(xmax)] + 0.1]
    overall[, xmax := wide[, max(xmax)] + 0.15]

    combine <- rbindlist(list(d, overall), use.names = TRUE, fill = TRUE)
    plot <- ggplot2::ggplot(
        combine,
        ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)
    ) +
        ggplot2::geom_rect(ggplot2::aes_string(fill = "group")) +
        ggplot2::scale_y_continuous(labels = scales::percent_format(), expand = c(0, 0)) +
        ggplot2::scale_x_continuous(breaks = breaks, expand = c(0, 0)) +
        ggplot2::guides(fill = ggplot2::guide_legend(reverse = TRUE)) +
        ggplot2::theme_bw() +
        ggplot2::theme(
            panel.grid = ggplot2::element_blank(),
            axis.text.x = ggplot2::element_blank(),
            legend.position = "bottom"
        ) +
        ggplot2::labs(fill = NULL)
    if (order == "segregation") {
        plot <- plot + ggplot2::labs(x = "< more segregated | less segregated >")
    }
    plot
}