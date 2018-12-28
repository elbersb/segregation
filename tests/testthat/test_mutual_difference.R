library(segregation)
context("test_mutual_diff")

test_data1 <- data.frame(
    u = c(rep("a", 4), rep("b", 4)),
    g = rep(c(1, 2, 3, 4), 2),
    n = c(40, 30, 20, 10, 5, 10, 15, 20)
)
test_data2 <- data.frame(
    u = c(rep("a", 4), rep("b", 4)),
    g = rep(c(1, 2, 3, 4), 2),
    n = c(20, 20, 20, 15, 5, 10, 10, 15)
)

test_that("mutual_difference method not defined error", {
    expect_error(mutual_difference(test_data1, test_data2, "g", "u", weight = "n", method = "X"))
})

test_that("mutual_difference SHAPLEY", {
    sh1 <- mutual_difference(test_data1, test_data2, "g", "u", weight = "n", method = "shapley")
    sh2 <- mutual_difference(test_data1, test_data2, "u", "g", weight = "n", method = "shapley")
    sh3 <- mutual_difference(test_data2, test_data1, "g", "u", weight = "n", method = "shapley")
    sh4 <- mutual_difference(test_data2, test_data1, "u", "g", weight = "n", method = "shapley")

    expect_equal(sh1[["diff", "est"]], sh1[["M2", "est"]] - sh1[["M1", "est"]])
    expect_equal(sh1[["diff", "est"]], sum(sh1[6:8, "est"]))
    expect_equal(sh1[["additions", "est"]], 0)
    expect_equal(sh1[["removals", "est"]], 0)

    expect_equal(sh2[["diff", "est"]], sh2[["M2", "est"]] - sh2[["M1", "est"]])
    expect_equal(sh2[["diff", "est"]], sum(sh2[6:8, "est"]))
    expect_equal(sh2[["additions", "est"]], 0)
    expect_equal(sh2[["removals", "est"]], 0)

    expect_equal(sh3[["diff", "est"]], sh3[["M2", "est"]] - sh3[["M1", "est"]])
    expect_equal(sh3[["diff", "est"]], sum(sh3[6:8, "est"]))
    expect_equal(sh3[["additions", "est"]], 0)
    expect_equal(sh3[["removals", "est"]], 0)

    expect_equal(sh4[["diff", "est"]], sh4[["M2", "est"]] - sh4[["M1", "est"]])
    expect_equal(sh4[["diff", "est"]], sum(sh4[6:8, "est"]))
    expect_equal(sh4[["additions", "est"]], 0)
    expect_equal(sh4[["removals", "est"]], 0)

    # symmetry by group/unit
    expect_equal(sh1[["M1", "est"]], sh2[["M1", "est"]])
    expect_equal(sh1[["M2", "est"]], sh2[["M2", "est"]])
    expect_equal(sh1[["diff", "est"]], sh2[["diff", "est"]])
    expect_equal(sh1[["group_marginal", "est"]], sh2[["unit_marginal", "est"]], tolerance = .0001)
    expect_equal(sh1[["unit_marginal", "est"]], sh2[["group_marginal", "est"]], tolerance = .0001)
    expect_equal(sh1[["structural", "est"]], sh2[["structural", "est"]], tolerance = .0001)

    expect_equal(sh3[["M1", "est"]], sh4[["M1", "est"]])
    expect_equal(sh3[["M2", "est"]], sh4[["M2", "est"]])
    expect_equal(sh3[["diff", "est"]], sh4[["diff", "est"]])
    expect_equal(sh3[["group_marginal", "est"]], sh4[["unit_marginal", "est"]], tolerance = .0001)
    expect_equal(sh3[["unit_marginal", "est"]], sh4[["group_marginal", "est"]], tolerance = .0001)
    expect_equal(sh3[["structural", "est"]], sh4[["structural", "est"]], tolerance = .0001)

    # symmetry by data inputs
    expect_equal(sh1[["M1", "est"]], sh3[["M2", "est"]])
    expect_equal(sh1[["M2", "est"]], sh3[["M1", "est"]])
    expect_equal(sh1[["diff", "est"]], -sh3[["diff", "est"]])
    expect_equal(sh1[["group_marginal", "est"]], -sh3[["group_marginal", "est"]], tolerance = .0001)
    expect_equal(sh1[["unit_marginal", "est"]], -sh3[["unit_marginal", "est"]], tolerance = .0001)
    expect_equal(sh1[["structural", "est"]], -sh3[["structural", "est"]], tolerance = .0001)

    expect_equal(sh2[["M1", "est"]], sh4[["M2", "est"]])
    expect_equal(sh2[["M2", "est"]], sh4[["M1", "est"]])
    expect_equal(sh2[["diff", "est"]], -sh4[["diff", "est"]])
    expect_equal(sh2[["group_marginal", "est"]], -sh4[["group_marginal", "est"]], tolerance = .0001)
    expect_equal(sh2[["unit_marginal", "est"]], -sh4[["unit_marginal", "est"]], tolerance = .0001)
    expect_equal(sh2[["structural", "est"]], -sh4[["structural", "est"]], tolerance = .0001)
})

test_that("mutual_difference KM", {
    ret1 <- mutual_difference(test_data1, test_data2, "g", "u", weight = "n", method = "km",
        precision = .0001)

    expect_equal(ret1[["diff", "est"]], ret1[["M2", "est"]] - ret1[["M1", "est"]])
    diff <- sum(ret1[c("unit_marginal", "group_marginal", "interaction", "structural"), "est"])
    expect_equal(ret1[["diff", "est"]], diff)
    expect_equal(ret1[["additions", "est"]], 0)
    expect_equal(ret1[["removals", "est"]], 0)

    # other way around
    ret <- mutual_difference(test_data1, test_data2, "u", "g", weight = "n", method = "km",
        precision = .0001)

    expect_equal(ret[["diff", "est"]], ret[["M2", "est"]] - ret[["M1", "est"]])
    expect_equal(ret[["diff", "est"]],
                 sum(ret[c("unit_marginal", "group_marginal", "interaction", "structural"), "est"]))

    # same as mutual_total
    expect_equal(ret[["M1", "est"]], mutual_total(test_data1, "u", "g", weight = "n")[["M", "est"]])
    expect_equal(ret[["M2", "est"]], mutual_total(test_data2, "u", "g", weight = "n")[["M", "est"]])

    expect_equal(nrow(ret), 9)
    expect_equal(ncol(ret), 2)

    # symmetrical
    ret$est <- round(ret$est, 5)
    ret1$est <- round(ret1$est, 5)
    expect_equal(ret[["M1", "est"]], ret1[["M1", "est"]])
    expect_equal(ret[["M2", "est"]], ret1[["M2", "est"]])
    expect_equal(ret[["diff", "est"]], ret1[["diff", "est"]])
    expect_equal(ret[["additions", "est"]], ret1[["additions", "est"]])
    expect_equal(ret[["removals", "est"]], ret1[["removals", "est"]])
    expect_equal(ret[["unit_marginal", "est"]], ret1[["group_marginal", "est"]])
    expect_equal(ret[["group_marginal", "est"]], ret1[["unit_marginal", "est"]])
    expect_equal(ret[["interaction", "est"]], ret1[["interaction", "est"]])
    expect_equal(ret[["structural", "est"]], ret1[["structural", "est"]])
})


test_that("mutual_difference MRC", {
    ret <- mutual_difference(test_data1, test_data2,
                             "g", "u", weight = "n", method = "mrc")

    expect_equal(ret[["diff", "est"]], ret[["M2", "est"]] - ret[["M1", "est"]])
    expect_equal(ret[["diff", "est"]],
                 sum(ret[c("unit_marginal", "group_marginal", "structural"), "est"]))
    expect_equal(ret[["additions", "est"]], 0)
    expect_equal(ret[["removals", "est"]], 0)

    # other way around
    ret <- mutual_difference(test_data1, test_data2,
                             "u", "g", weight = "n", method = "mrc")

    expect_equal(ret[["diff", "est"]], ret[["M2", "est"]] - ret[["M1", "est"]])
    expect_equal(ret[["diff", "est"]],
                 sum(ret[c("unit_marginal", "group_marginal", "structural"), "est"]))

    # same as mutual_total
    expect_equal(ret[["M1", "est"]], mutual_total(test_data1, "u", "g", weight = "n")[["M", "est"]])
    expect_equal(ret[["M2", "est"]], mutual_total(test_data2, "u", "g", weight = "n")[["M", "est"]])

    expect_equal(nrow(ret), 8)
    expect_equal(ncol(ret), 2)
})


test_that("mutual_difference SE", {
    ret <- mutual_difference(test_data1, test_data2, "g", "u", weight = "n",
                             method = "shapley", se = TRUE, n_bootstrap = 5)
    expect_equal(nrow(ret), 8)
    expect_equal(ncol(ret), 3)
    expect_equal(all(ret[ret$est > 0, "se"] > 0), TRUE)

    ret <- mutual_difference(test_data1, test_data2, "g", "u", weight = "n",
                             method = "km", se = TRUE, n_bootstrap = 5)
    expect_equal(nrow(ret), 9)
    expect_equal(ncol(ret), 3)
    expect_equal(all(ret[ret$est > 0, "se"] > 0), TRUE)

    ret <- mutual_difference(test_data1, test_data2, "g", "u", weight = "n",
                             method = "mrc", se = TRUE, n_bootstrap = 5)
    expect_equal(nrow(ret), 8)
    expect_equal(ncol(ret), 3)
    expect_equal(all(ret[ret$est > 0, "se"] > 0), TRUE)
})


test_that("mutual_difference log base", {
    ret <- mutual_difference(test_data1, test_data2, "g", "u",
                             weight = "n", method = "shapley", base = 2)
    expect_equal(ret[["diff", "est"]], ret[["M2", "est"]] - ret[["M1", "est"]])
    expect_equal(nrow(ret), 8)
    expect_equal(ncol(ret), 2)

    ret <- mutual_difference(test_data1, test_data2, "g", "u",
                             weight = "n", method = "km", base = 2)
    expect_equal(ret[["diff", "est"]], ret[["M2", "est"]] - ret[["M1", "est"]])
    expect_equal(nrow(ret), 9)
    expect_equal(ncol(ret), 2)

    ret <- mutual_difference(test_data1, test_data2, "g", "u",
                             weight = "n", method = "mrc", base = 2)
    expect_equal(ret[["diff", "est"]], ret[["M2", "est"]] - ret[["M1", "est"]])
    expect_equal(nrow(ret), 8)
    expect_equal(ncol(ret), 2)
})


test_that("difference same as mutual_total (zero weights)", {
    # test with zero weights
    test_data1 <- data.frame(
        u = c(rep("a", 4), rep("b", 4), rep("c", 4)),
        g = rep(c(1, 2, 3, 4), 3),
        n = c(0, 30, 20, 10, 5, 10, 15, 20, 4, 3, 2, 1)
    )
    test_data2 <- data.frame(
        u = c(rep("a", 4), rep("b", 4), rep("d", 4)),
        g = rep(c(1, 2, 3, 4), 3),
        n = c(40, 20, 20, 15, 5, 10, 10, 0, 1, 2, 3, 4)
    )

    shapley <- mutual_difference(test_data1, test_data2,
                             "g", "u", weight = "n", method = "shapley")
    km <- mutual_difference(test_data1, test_data2,
                             "g", "u", weight = "n", method = "km")
    mrc <- mutual_difference(test_data1, test_data2,
                                 "g", "u", weight = "n", method = "mrc")
    M1 <- mutual_total(test_data1, "g", "u", weight = "n")[["M", "est"]]
    M2 <- mutual_total(test_data2, "g", "u", weight = "n")[["M", "est"]]

    expect_equal(shapley[["M1", "est"]], M1)
    expect_equal(shapley[["M2", "est"]], M2)
    expect_equal(km[["M1", "est"]], M1)
    expect_equal(km[["M2", "est"]], M2)
    expect_equal(mrc[["M1", "est"]], M1)
    expect_equal(mrc[["M2", "est"]], M2)

    # very small weights
    test_data1$n2 <- test_data1$n / 1000
    test_data2$n2 <- test_data2$n

    shapley2 <- mutual_difference(test_data1, test_data2,
                                  "g", "u", weight = "n2", method = "shapley")
    expect_equal(shapley[["structural", "est"]], shapley2[["structural", "est"]],
                 tolerance = .001)
})


test_that("correctly identifies marginal/structural changes", {
    test_data <- data.frame(
        u = c(rep("a", 2), rep("b", 2), rep("c", 2)),
        g = rep(c(1, 2), 3),
        n = c(25, 25, 28, 2, 2, 18)
    )
    test_data_cols <- data.frame(
        u = c(rep("a", 2), rep("b", 2), rep("c", 2)),
        g = rep(c(1, 2), 3),
        n = c(25, 25, 28, 2, 2 * 2, 18 * 2)
    )
    test_data_rows <- data.frame(
        u = c(rep("a", 2), rep("b", 2), rep("c", 2)),
        g = rep(c(1, 2), 3),
        n = c(25, 25 * 2, 28, 2 * 2, 2, 18 * 2)
    )

    shapley_cols <- mutual_difference(test_data, test_data_cols,
                             "g", "u", weight = "n", method = "shapley")
    expect_equal(max(abs(shapley_cols[6:8, ][["est"]])),
                 abs(shapley_cols[["unit_marginal", "est"]]))
    expect_equal(shapley_cols[["structural", "est"]], 0, tolerance = .001)

    shapley_rows <- mutual_difference(test_data, test_data_rows,
                             "g", "u", weight = "n", method = "shapley")
    expect_equal(max(abs(shapley_rows[6:8, ][["est"]])),
                 abs(shapley_rows[["unit_marginal", "est"]]))
    expect_equal(shapley_rows[["structural", "est"]], 0, tolerance = .001)
})
