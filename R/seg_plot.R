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
#' @param order A character, either "segregation" or "majority". Affects
#'   the ordering of the units.
#' @return Returns a ggplot2 object.
#' @import data.table
#' @export
seg_plot <- function(data, group, unit, weight, order = "segregation") {
    if (!requireNamespace("ggplot2", quietly = TRUE)) {
        stop("Please install ggplot2 to use this function")
    }

    stopifnot(length(group) == 1)
    stopifnot(order %in% c("segregation", "majority"))
    d <- prepare_data(data, group, unit, weight)
    # easier if renamed
    setnames(d, group, "group")

    if (is.factor(d[["group"]])) {
        d[, group := as.character(group)]
    }

    d[, p := freq / sum(freq), by = unit]
    d[, p_unit := sum(freq), by = unit]
    N <- d[, first(p_unit), by = unit][, sum(V1)]
    d[, p_unit := p_unit / N]

    # overall
    overall <- d[, .(freq = sum(freq)), by = .(group)]
    overall[, p := freq / sum(freq)]
    setorder(overall, -p)
    group_order <- overall[["group"]]

    form <- paste("p_unit +", paste(unit, collapse = "+"), "~ group")
    wide <- data.table::dcast(d[, -"freq"], form, value.var = "p", fill = 0)

    if (order == "segregation") {
        ls <- mutual_local(d, "group", unit, weight = "freq", wide = TRUE)
        wide <- merge(ls, wide, by = unit)
        setorder(wide, -ls)
    } else if (order == "majority") {
        setorderv(wide,
            c(group_order[[1]], utils::tail(group_order, 1)),
            order = c(1, -1)
        )
    }

    # format units
    wide[, xmin := cumsum(p_unit) - p_unit]
    wide[, xmax := cumsum(p_unit)]
    cols <- c(unit, "xmin", "xmax")
    d <- merge(d, wide[, ..cols], by = unit)
    d[, group := factor(group, levels = group_order)]
    setorderv(d, c("xmin", "group"))
    d[, ymin := cumsum(p) - p, by = unit]
    d[, ymax := cumsum(p), by = unit]
    breaks <- c(wide[["xmin"]], 1)

    # format overall
    overall[, group := factor(group, levels = group_order)]
    overall[, ymin := cumsum(p) - p]
    overall[, ymax := cumsum(p)]
    overall[, xmin := 1.05]
    overall[, xmax := 1.08]

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