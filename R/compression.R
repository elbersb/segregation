#' Compresses a segregation matrix
#'
#' Given a data set that identifies suitable neighbors for merging,
#' this function will merge units iteratively, where in each iteration
#' the neighbors with the smallest effect on total M will be merged.
#'
#' @param data A data frame.
#' @param neighbors A data frame with exactly two columns. Each row identifies
#'   a set of "neighbors" that may be merged.
#' @param group A categorical variable
#'   contained in \code{data}. Defines the first dimension
#'   over which segregation is computed.
#' @param unit A categorical variable
#'   contained in \code{data}. Defines the second dimension
#'   over which segregation is computed.
#' @param weight Numeric. Only frequency weights are allowed.
#'   (Default \code{NULL})
#' @param max_iter Maximum number of iterations (Default \code{Inf})
#' @return Returns a data.table.
#' @import data.table
#' @export
compress <- function(data, neighbors, group, unit, weight = NULL, max_iter = Inf) {
    stopifnot(ncol(neighbors) == 2)
    stopifnot(length(group) == 1)
    stopifnot(length(unit) == 1)

    data <- as.data.table(data)
    cols <- c(group, unit, weight)
    cols_to_delete <- names(data)[!names(data) %in% cols]
    if (length(cols_to_delete) > 0)
        set(data, j = cols_to_delete, value = NULL)

    # rename -- easier
    setnames(data, unit, "unit")
    setnames(data, group, "group")
    setkeyv(data, "unit")
    if (is.null(weight)) {
        weight <- "weight"
        data[, weight := 1]
    } else {
        setnames(data, weight, "weight")
    }

    if (is.factor(data[, unit])) {
        data[, unit := as.character(unit)]
    }
    if (is.factor(data[, group])) {
        data[, group := as.character(group)]
    }

    # complete data table
    data <- merge(CJ(unit = data[, unique(unit)], group = data[, unique(group)]), data,
        by = c("unit", "group"), all = TRUE)
    data[is.na(weight), weight := 0]

    # calculate M
    current_M <- mutual_total(data, "group", "unit", weight = "weight")$est[[1]]
    initial_M <- current_M
    total_N <- data[, sum(weight)]

    total_units <- length(data[, unique(unit)])
    total_groups <- length(data[, unique(group)])

    # sort within rows to get rid of duplicates (i.e. 1-2 eq. 2-1)
    neighbors <- data.table::as.data.table(t(apply(neighbors, 1, sort)))
    neighbors <- unique(neighbors)[V1 != V2]
    neighbors[, pair := 1:.N]

    nd <- merge(neighbors, data, by.x = "V1", by.y = "unit",
                allow.cartesian = TRUE)
    setnames(nd, "weight", "freq1")

    nd <- merge(nd, data, by.x = c("group", "V2"), by.y = c("group", "unit"),
                allow.cartesian = TRUE)
    setnames(nd, "weight", "freq2")

    # find lowest distance
    nd[, total := sum(freq1 + freq2), by = "pair"]
    # could keep total unit counts to save one grouping here
    nd[, `:=`(
        p_unit1 = sum(freq1) / total,
        p_unit2 = sum(freq2) / total), by = "pair"]
    nd[, `:=`(
        p_unit_g_group1 = freq1 / (freq1 + freq2),
        p_unit_g_group2 = freq2 / (freq1 + freq2),
        p_group = (freq1 + freq2) / total)]
    nd[, `:=`(
        ls1 = p_unit_g_group1 * log(p_unit_g_group1 / p_unit1),
        ls2 = p_unit_g_group2 * log(p_unit_g_group2 / p_unit2))]
    nd[is.na(ls1), ls1 := 0]
    nd[is.na(ls2), ls2 := 0]
    nd[, M := sum(p_group * (ls1 + ls2)), by = "pair"]
    nd[, M_wgt := total / total_N * M]
    setorder(nd, V1, V2)

    # store results
    if (is.infinite(max_iter)) {
        max_iter <- min(nrow(neighbors), total_units - 1)
    }
    res <- vector(mode = "list")
    pb <- utils::txtProgressBar(min = 0, max = max_iter, style = 3)

    for (i in 1:max_iter) {
        utils::setTxtProgressBar(pb, i)

        # get minimum
        min = nd[order(M_wgt)][1, .(V1, V2, pair, M_wgt)]
        M_wgt <- min[, M_wgt]
        current_M <- current_M - M_wgt
        old_unit <- min[, V1]
        new_unit <- min[, V2]

        # update names
        nd[V1 == old_unit, V1 := new_unit]
        nd[V2 == old_unit, V2 := new_unit]

        # get new n values for merged pair
        merge_pair <- nd[pair == min[, pair]]
        stopifnot(nrow(merge_pair) == total_groups)
        pair1 <- merge_pair[, .(V1 = new_unit, freq1 = freq1 + freq2, group)]
        pair2 <- merge_pair[, .(V2 = new_unit, freq2 = freq1 + freq2, group)]

        # update n values for merged unit
        nd[pair1, freq1 := i.freq1, on = c("V1", "group")]
        nd[pair2, freq2 := i.freq2, on = c("V2", "group")]

        nd[, pair := NULL]
        nd <- unique(nd[V1 != V2])

        if (nrow(nd) > 0) {
            nd[, pair := rep.int(1:(.N / total_groups), rep(total_groups, .N / total_groups))]

            setkey(nd, pair)
            pairs <- nd[V1 == new_unit | V2 == new_unit, unique(pair)]

            # update
            nd[.(pairs), total := sum(freq1 + freq2), by = "pair"]

            nd[.(pairs), `:=`(
                p_unit1 = sum(freq1) / total,
                p_unit2 = sum(freq2) / total), by = "pair"]
            nd[.(pairs), `:=`(
                p_unit_g_group1 = freq1 / (freq1 + freq2),
                p_unit_g_group2 = freq2 / (freq1 + freq2),
                p_group = (freq1 + freq2) / total)]
            nd[.(pairs), `:=`(
                ls1 = p_unit_g_group1 * log(p_unit_g_group1 / p_unit1),
                ls2 = p_unit_g_group2 * log(p_unit_g_group2 / p_unit2))]
            nd[is.na(ls1), ls1 := 0]
            nd[is.na(ls2), ls2 := 0]
            nd[.(pairs), M := sum(p_group * (ls1 + ls2)), by = "pair"]
            nd[.(pairs), M_wgt := total / total_N * M]
        }

        if (total_units - i == 1) {
            current_M <- 0
        }

        # record results for this iteration
        res[[i]] <- list(
            iter = i,
            M_wgt = M_wgt,
            M = current_M,
            N_units = total_units - i,
            old_unit = old_unit,
            new_unit = new_unit,
            time = Sys.time()
        )

        if (nrow(nd) == 0) {
            break
        }
    }
    utils::setTxtProgressBar(pb, max_iter)
    close(pb)

    data.table::rbindlist(res)
}

#' Takes the results from \code{compress} and returns a crosswalk
#'
#' Given the results from \code{compress}, this function can be used
#' to get a crosswalk to identify merged units at a given iteration.
#'
#' @param iterations A data frame returned by the \code{compress} function.
#' @param n_iterations The number of iterations to take into account when
#'   constructing the crosswalk.'
#' @return Returns a data.table.
#' @import data.table
#' @export
compress_crosswalk <- function(iterations, n_iterations = Inf) {
    if (!"data.frame" %in% class(iterations) |
            !all(c("N_units", "old_unit", "new_unit") %in% names(iterations))) {
        stop("iterations needs to be an object returned from the `compress` function")
    }
    if (is.finite(n_iterations) & n_iterations <= 0) {
        stop("n_iterations has to be larger than 0")
    }
    if (is.finite(n_iterations) & n_iterations > nrow(iterations)) {
        stop("n_iterations is too large")
    }

    if (is.infinite(n_iterations)) {
        n_iterations <- nrow(iterations)
    }

    iterations <- as.data.table(iterations[1:n_iterations,
        c("N_units", "old_unit", "new_unit")])
    n <- iterations[, utils::tail(N_units, 1)]
    iterations[, N_units := NULL]

    bags <- list(c(iterations[1, old_unit], iterations[1, new_unit]))
    if (nrow(iterations) > 1) {
        for (i in 2:nrow(iterations)) {
            old_unit <- iterations[i, old_unit]
            new_unit <- iterations[i, new_unit]

            old_unit_bag <- sapply(1:length(bags), function(ibag) old_unit %in% bags[[ibag]])
            new_unit_bag <- sapply(1:length(bags), function(ibag) new_unit %in% bags[[ibag]])

            if ((sum(old_unit_bag) + sum(new_unit_bag)) == 0) {
                bags[[length(bags) + 1]] <- c(old_unit, new_unit)
            } else if (sum(old_unit_bag) == 1 & sum(new_unit_bag) == 0) {
                bags[[which(old_unit_bag)]] <- c(bags[[which(old_unit_bag)]], new_unit)
            } else if (sum(old_unit_bag) == 0 & sum(new_unit_bag) == 1) {
                bags[[which(new_unit_bag)]] <- c(bags[[which(new_unit_bag)]], old_unit)
            } else if (sum(old_unit_bag) == 1 & sum(new_unit_bag) == 1) {
                bags[[length(bags) + 1]] <- c(bags[[which(new_unit_bag)]],
                    bags[[which(old_unit_bag)]])
                bags[[which(new_unit_bag)]] <- "X"
                bags[[which(old_unit_bag)]] <- "X"
                bags <- bags[bags != "X"]
            }
        }
    }

    data.table(unit = unlist(bags),
        new = paste0("M", rep(1:length(bags), lengths(bags))),
        parts = rep(sapply(bags, function(x) paste(sort(x), collapse = "/")), lengths(bags)))
}
