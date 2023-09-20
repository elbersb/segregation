if (!identical(Sys.getenv("NOT_CRAN"), "true")) {
    return()
}

library("segregation")
context("test_compression")

subset <- schools00[1:50, ]
data.table::setDT(subset)
n_schools <- length(unique(subset$school))

all_neighbors <- unique(subset$school)
all_neighbors <- expand.grid(a = all_neighbors, b = all_neighbors)
res_all <- compress(subset, "race", "school", weight = "n", neighbors = all_neighbors)

test_that("result is the same with no neighbors given", {
    res2 <- compress(subset, "race", "school", neighbors = "all", weight = "n")
    expect_equal(res_all$iterations, res2$iterations)
})

test_that("compress works", {
    # 9 merges
    expect_equal(nrow(res_all$iterations), 16)
    # M values is declining continously
    expect_equal(all(res_all$iterations$M[2:16] < res_all$M$iterations[1:15]), TRUE)
    # number of units are correct
    expect_equal(res_all$iteration$N_units[[1]], n_schools - 1)
})

test_that("print", {
    expect_output(print(res_all), "17 units")
    expect_output(print(res_all), "Threshold 99%")
})

test_that("get_crosswalk works", {
    expect_error(
        get_crosswalk(schools00),
        "either n_units or percent has to be given"
    )
    expect_error(
        get_crosswalk(res_all, n_units = -1),
        "n_units is out of bounds"
    )
    expect_error(
        get_crosswalk(res_all, n_units = 20),
        "n_units is out of bounds"
    )
    expect_error(
        get_crosswalk(res_all, n_units = 3, percent = 0.2),
        "only n_units or percent has to be given"
    )

    expect_equal(nrow(get_crosswalk(res_all, n_units = 1)), n_schools)
    expect_equal(nrow(get_crosswalk(res_all, n_units = 5)), n_schools)
    expect_equal(nrow(get_crosswalk(res_all, n_units = 15)), n_schools)
    expect_equal(nrow(get_crosswalk(res_all, percent = 0.1)), n_schools)
    expect_equal(nrow(get_crosswalk(res_all, percent = 0.6)), n_schools)
    expect_equal(nrow(get_crosswalk(res_all, percent = 0.9)), n_schools)
    expect_equal(as.character(get_crosswalk(res_all, n_units = 1)$new), rep("M1", 17))
})

test_that("parts", {
    # get_crosswalk
    res_no_parts <- get_crosswalk(res_all, percent = 0.6)
    res_parts <- get_crosswalk(res_all, percent = 0.6, parts = TRUE)
    expect_equal(names(res_no_parts), c("school", "new"))
    expect_equal(names(res_parts), c("school", "new", "parts"))
    expect_equal(res_no_parts, res_parts[, -"parts"])

    res_no_parts <- get_crosswalk(res_all, percent = 0.99)
    res_parts <- get_crosswalk(res_all, percent = 0.99, parts = TRUE)
    expect_equal(res_no_parts, res_parts[, -"parts"])

    expect_true(all(!is.na(res_parts[grepl("^M", new)][["parts"]])))

    # merge_units
    merged_no_parts <- merge_units(res_all, percent = .8)
    merged_parts <- merge_units(res_all, percent = .8, parts = TRUE)
    expect_equal(res_no_parts, res_parts[, -"parts"])
    expect_equal(names(merged_no_parts), c("school", "race", "n"))
    expect_equal(names(merged_parts), c("school", "race", "n", "parts"))
})

test_that("compress edge case", {
    res_edge <- compress(subset, "race", "school", neighbors = "all", weight = "n", max_iter = 1)
    expect_equal(nrow(get_crosswalk(res_edge, n_units = 16)), n_schools)
})

test_that("merge_units", {
    merged <- merge_units(res_all, percent = 0.8)
    new_units_cw <- sort(unique(get_crosswalk(res_all, percent = 0.8)$new))
    new_units_merged <- sort(unique(merged$school))
    expect_equal(new_units_cw, new_units_merged)
})

test_that("percent works", {
    M_full <- mutual_total(subset, "race", "school", weight = "n")[stat == "M"][["est"]]

    for (pct in seq(0.1, 0.9, by = 0.05)) {
        merged_pct <- merge_units(res_all, percent = pct)
        M_pct <- mutual_total(merged_pct, "race", "school", weight = "n")[stat == "M"][["est"]]
        pct_M <- M_pct / M_full
        expect_true(pct_M > pct)
        expect_equal(res_all$iterations[N_units == merged_pct[, uniqueN(school)]][["pct_M"]], pct_M)
    }
})

test_that("merge_units edge case", {
    res_edge <- compress(subset, "race", "school", neighbors = "all", weight = "n", max_iter = 1)
    merged <- merge_units(res_edge, n_units = 16)
    # replicate manual merge
    units <- c(res_edge$iterations$old_unit, res_edge$iterations$new_unit)
    merged_manually <- subset[school %in% units, .(n = sum(n)), by = .(race)]
    merged_algo <- merged[school == "M1" & n != 0][, -"school"]
    expect_equal(merged_manually, merged_algo)
})

test_that("scree plot", {
    if (requireNamespace("ggplot2", quietly = TRUE)) {
        plot <- scree_plot(res_all)
        expect_equal(nrow(plot$data), n_schools)

        plot <- scree_plot(res_all, tail = 3)
        expect_equal(nrow(plot$data), 3)
    }
})

test_that("data set names", {
    subset <- schools00[1:50, ]
    data.table::setDT(subset)
    names(subset) <- c("state", "district", "unit", "group", "n")
    res <- compress(subset, "group", "unit", neighbors = "all", weight = "n")

    expect_equal(nrow(res$iterations), 16)
})

test_that("local neighbors", {
    subset <- schools00[1:500, ]
    data.table::setDT(subset)
    res_local <- compress(subset, "race", "school", neighbors = "local", n_neighbors = 100, weight = "n")
    res_local_small <- compress(subset, "race", "school", neighbors = "local", n_neighbors = 5, weight = "n")
    res_all <- compress(subset, "race", "school", neighbors = "all", weight = "n")

    expect_equal(res_local$iterations$old_unit, res_all$iterations$old_unit)
    expect_true(
        res_local_small$iterations[pct_M > 0.99][.N][["N_units"]] >
            res_local$iterations[pct_M > 0.99][.N][["N_units"]]
    )
})

test_that("dendrogram", {
    dend <- as.dendrogram(res_all)
    expect_equal(attr(dend, "height"), res_all$iterations$M[[1]])
    expect_equal(attr(dend, "members"), length(unique(res_all$data$school)))

    res_limited <- compress(subset, "race", "school", weight = "n", max_iter = 5)
    expect_error(as.dendrogram(res_limited))
})
