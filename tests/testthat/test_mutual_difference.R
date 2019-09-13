library("segregation")
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

    expect_equal(sh1[stat == "diff", est], sh1[stat == "M2", est] - sh1[stat == "M1", est])
    expect_equal(sh1[stat == "diff", est], sum(sh1[6:8, "est"]))
    expect_equal(sh1[stat == "additions", est], 0)
    expect_equal(sh1[stat == "removals", est], 0)

    expect_equal(sh2[stat == "diff", est], sh2[stat == "M2", est] - sh2[stat == "M1", est])
    expect_equal(sh2[stat == "diff", est], sum(sh2[6:8, "est"]))
    expect_equal(sh2[stat == "additions", est], 0)
    expect_equal(sh2[stat == "removals", est], 0)

    expect_equal(sh3[stat == "diff", est], sh3[stat == "M2", est] - sh3[stat == "M1", est])
    expect_equal(sh3[stat == "diff", est], sum(sh3[6:8, "est"]))
    expect_equal(sh3[stat == "additions", est], 0)
    expect_equal(sh3[stat == "removals", est], 0)

    expect_equal(sh4[stat == "diff", est], sh4[stat == "M2", est] - sh4[stat == "M1", est])
    expect_equal(sh4[stat == "diff", est], sum(sh4[6:8, "est"]))
    expect_equal(sh4[stat == "additions", est], 0)
    expect_equal(sh4[stat == "removals", est], 0)

    # symmetry by group/unit
    expect_equal(sh1[stat == "M1", est], sh2[stat == "M1", est])
    expect_equal(sh1[stat == "M2", est], sh2[stat == "M2", est])
    expect_equal(sh1[stat == "diff", est], sh2[stat == "diff", est])
    expect_equal(sh1[stat == "group_marginal", est], sh2[stat == "unit_marginal", est],
        tolerance = .0001)
    expect_equal(sh1[stat == "unit_marginal", est], sh2[stat == "group_marginal", est],
        tolerance = .0001)
    expect_equal(sh1[stat == "structural", est], sh2[stat == "structural", est],
        tolerance = .0001)

    expect_equal(sh3[stat == "M1", est], sh4[stat == "M1", est])
    expect_equal(sh3[stat == "M2", est], sh4[stat == "M2", est])
    expect_equal(sh3[stat == "diff", est], sh4[stat == "diff", est])
    expect_equal(sh3[stat == "group_marginal", est], sh4[stat == "unit_marginal", est],
        tolerance = .0001)
    expect_equal(sh3[stat == "unit_marginal", est], sh4[stat == "group_marginal", est],
        tolerance = .0001)
    expect_equal(sh3[stat == "structural", est], sh4[stat == "structural", est],
        tolerance = .0001)

    # symmetry by data inputs
    expect_equal(sh1[stat == "M1", est], sh3[stat == "M2", est])
    expect_equal(sh1[stat == "M2", est], sh3[stat == "M1", est])
    expect_equal(sh1[stat == "diff", est], -sh3[stat == "diff", est])
    expect_equal(sh1[stat == "group_marginal", est], -sh3[stat == "group_marginal", est],
        tolerance = .0001)
    expect_equal(sh1[stat == "unit_marginal", est], -sh3[stat == "unit_marginal", est],
        tolerance = .0001)
    expect_equal(sh1[stat == "structural", est], -sh3[stat == "structural", est],
        tolerance = .0001)

    expect_equal(sh2[stat == "M1", est], sh4[stat == "M2", est])
    expect_equal(sh2[stat == "M2", est], sh4[stat == "M1", est])
    expect_equal(sh2[stat == "diff", est], -sh4[stat == "diff", est])
    expect_equal(sh2[stat == "group_marginal", est], -sh4[stat == "group_marginal", est],
        tolerance = .0001)
    expect_equal(sh2[stat == "unit_marginal", est], -sh4[stat == "unit_marginal", est],
        tolerance = .0001)
    expect_equal(sh2[stat == "structural", est], -sh4[stat == "structural", est],
        tolerance = .0001)
})

test_that("mutual_difference KM", {
    ret1 <- mutual_difference(test_data1, test_data2, "g", "u", weight = "n", method = "km",
        precision = .0001)

    expect_equal(ret1[stat == "diff", est], ret1[stat == "M2", est] - ret1[stat == "M1", est])
    diff <- ret1[stat %in% c("unit_marginal", "group_marginal", "interaction", "structural"),
        sum(est)]
    expect_equal(ret1[stat == "diff", est], diff)
    expect_equal(ret1[stat == "additions", est], 0)
    expect_equal(ret1[stat == "removals", est], 0)

    # other way around
    ret <- mutual_difference(test_data1, test_data2, "u", "g", weight = "n", method = "km",
        precision = .0001)

    expect_equal(ret[stat == "diff", est], ret[stat == "M2", est] - ret[stat == "M1", est])
    expect_equal(ret[stat == "diff", est],
        ret[stat %in% c("unit_marginal", "group_marginal", "interaction", "structural"), sum(est)])

    # same as mutual_total
    expect_equal(ret[stat == "M1", est],
        mutual_total(test_data1, "u", "g", weight = "n")[stat == "M", est])
    expect_equal(ret[stat == "M2", est],
        mutual_total(test_data2, "u", "g", weight = "n")[stat == "M", est])

    expect_equal(nrow(ret), 9)
    expect_equal(ncol(ret), 2)

    # symmetrical
    ret$est <- round(ret$est, 5)
    ret1$est <- round(ret1$est, 5)
    expect_equal(ret[stat == "M1", est], ret1[stat == "M1", est])
    expect_equal(ret[stat == "M2", est], ret1[stat == "M2", est])
    expect_equal(ret[stat == "diff", est], ret1[stat == "diff", est])
    expect_equal(ret[stat == "additions", est], ret1[stat == "additions", est])
    expect_equal(ret[stat == "removals", est], ret1[stat == "removals", est])
    expect_equal(ret[stat == "unit_marginal", est], ret1[stat == "group_marginal", est])
    expect_equal(ret[stat == "group_marginal", est], ret1[stat == "unit_marginal", est])
    expect_equal(ret[stat == "interaction", est], ret1[stat == "interaction", est])
    expect_equal(ret[stat == "structural", est], ret1[stat == "structural", est])
})


test_that("mutual_difference MRC", {
    ret <- mutual_difference(test_data1, test_data2,
                             "g", "u", weight = "n", method = "mrc")

    expect_equal(ret[stat == "diff", est], ret[stat == "M2", est] - ret[stat == "M1", est])
    expect_equal(ret[stat == "diff", est],
                 ret[stat %in% c("unit_marginal", "group_marginal", "structural"), sum(est)])
    expect_equal(ret[stat == "additions", est], 0)
    expect_equal(ret[stat == "removals", est], 0)

    # other way around
    ret <- mutual_difference(test_data1, test_data2,
                             "u", "g", weight = "n", method = "mrc")

    expect_equal(ret[stat == "diff", est], ret[stat == "M2", est] - ret[stat == "M1", est])
    expect_equal(ret[stat == "diff", est],
                 ret[stat %in% c("unit_marginal", "group_marginal", "structural"), sum(est)])

    # same as mutual_total
    expect_equal(ret[stat == "M1", est],
        mutual_total(test_data1, "u", "g", weight = "n")[stat == "M", est])
    expect_equal(ret[stat == "M2", est],
        mutual_total(test_data2, "u", "g", weight = "n")[stat == "M", est])

    expect_equal(nrow(ret), 8)
    expect_equal(ncol(ret), 2)
})


test_that("mutual_difference SE", {
    ret <- mutual_difference(test_data1, test_data2, "g", "u", weight = "n",
                             method = "shapley", se = TRUE, n_bootstrap = 5)
    expect_equal(nrow(ret), 8)
    expect_equal(ncol(ret), 3)
    expect_equal(all(ret[est > 0, se] > 0), TRUE)

    ret <- mutual_difference(test_data1, test_data2, "g", "u", weight = "n",
                             method = "km", se = TRUE, n_bootstrap = 5)
    expect_equal(nrow(ret), 9)
    expect_equal(ncol(ret), 3)
    expect_equal(all(ret[est > 0, se] > 0), TRUE)

    ret <- mutual_difference(test_data1, test_data2, "g", "u", weight = "n",
                             method = "mrc", se = TRUE, n_bootstrap = 5)
    expect_equal(nrow(ret), 8)
    expect_equal(ncol(ret), 3)
    expect_equal(all(ret[est > 0, se] > 0), TRUE)
})

test_that("mutual_difference SHAPLEY detailed", {
    diff_simple <- mutual_difference(schools05, schools00, group = "school", unit = "race",
        weight = "n", method = "shapley", precision = .000001)
    # note that the detailed decomposition requires high precision
    diff <- mutual_difference(schools05, schools00, group = "school", unit = "race",
        weight = "n", method = "shapley_detailed", precision = .000001)

    expect_equal(nrow(diff_simple), 8)
    expect_equal(ncol(diff_simple), 2)
    expect_equal(nrow(diff), 8 + length(unique(schools00$race)) * 5)
    expect_equal(ncol(diff), 3)

    # same with and without detail
    expect_equal(diff_simple[, est], diff[1:8, est], tolerance = .00001)

    wide <- dcast(diff[!is.na(race), ], race ~ stat, value.var = "est")
    expect_equal(wide[, sum(.5 * p1 * ls_diff1 + .5 * p2 * ls_diff2)], wide[, sum(total)])
    expect_equal(wide[, sum(.5 * p1 * ls_diff1 + .5 * p2 * ls_diff2)],
        diff[stat == "structural", est], tolerance = .00001)

    # reverse units and groups
    diff <- mutual_difference(schools05, schools00, group = "race", unit = "school",
        weight = "n", method = "shapley_detailed", precision = .000001)

    schools_in_common <- intersect(schools00$school, schools05$school)
    expect_equal(nrow(diff), 8 + length(schools_in_common) * 5)
    expect_equal(ncol(diff), 3)

    wide <- dcast(diff[!is.na(school), ], school ~ stat, value.var = "est")
    expect_equal(wide[, sum(.5 * p1 * ls_diff1 + .5 * p2 * ls_diff2)], wide[, sum(total)])
    expect_equal(wide[, sum(.5 * p1 * ls_diff1 + .5 * p2 * ls_diff2)],
        diff[stat == "structural", est], tolerance = .001)
})


test_that("mutual_difference SHAPLEY detailed with SE", {
    diff <- mutual_difference(schools05, schools00, group = "school", unit = "race",
        weight = "n", method = "shapley_detailed", precision = .1, se = TRUE, n_bootstrap = 2)

    expect_equal(nrow(diff), 8 + length(unique(schools00$race)) * 5)
    expect_equal(ncol(diff), 4)
})


test_that("mutual_difference log base", {
    ret <- mutual_difference(test_data1, test_data2, "g", "u",
                             weight = "n", method = "shapley", base = 2)
    expect_equal(ret[stat == "diff", est], ret[stat == "M2", est] - ret[stat == "M1", est])
    expect_equal(nrow(ret), 8)
    expect_equal(ncol(ret), 2)

    ret <- mutual_difference(test_data1, test_data2, "g", "u",
                             weight = "n", method = "km", base = 2)
    expect_equal(ret[stat == "diff", est], ret[stat == "M2", est] - ret[stat == "M1", est])
    expect_equal(nrow(ret), 9)
    expect_equal(ncol(ret), 2)

    ret <- mutual_difference(test_data1, test_data2, "g", "u",
                             weight = "n", method = "mrc", base = 2)
    expect_equal(ret[stat == "diff", est], ret[stat == "M2", est] - ret[stat == "M1", est])
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
    M1 <- mutual_total(test_data1, "g", "u", weight = "n")[stat == "M", est]
    M2 <- mutual_total(test_data2, "g", "u", weight = "n")[stat == "M", est]

    expect_equal(shapley[stat == "M1", est], M1)
    expect_equal(shapley[stat == "M2", est], M2)
    expect_equal(km[stat == "M1", est], M1)
    expect_equal(km[stat == "M2", est], M2)
    expect_equal(mrc[stat == "M1", est], M1)
    expect_equal(mrc[stat == "M2", est], M2)

    # very small weights
    test_data1$n2 <- test_data1$n / 1000
    test_data2$n2 <- test_data2$n

    shapley2 <- mutual_difference(test_data1, test_data2,
                                  "g", "u", weight = "n2", method = "shapley")
    expect_equal(shapley[stat == "structural", est], shapley2[stat == "structural", est],
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
    expect_equal(max(abs(shapley_cols[6:8, est])),
                 abs(shapley_cols[stat == "unit_marginal", est]))
    expect_equal(shapley_cols[stat == "structural", est], 0, tolerance = .001)

    shapley_rows <- mutual_difference(test_data, test_data_rows,
                             "g", "u", weight = "n", method = "shapley")
    expect_equal(max(abs(shapley_rows[6:8, est])),
                 abs(shapley_rows[stat == "unit_marginal", est]))
    expect_equal(shapley_rows[stat == "structural", est], 0, tolerance = .001)
})
