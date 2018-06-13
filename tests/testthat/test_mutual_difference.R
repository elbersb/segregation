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

test_that("mutual_difference IPF", {
    ret1 <- mutual_difference(test_data1, test_data2, "g", "u", weight = "n", method = "ipf")

    expect_equal(ret1[["diff", "est"]], ret1[["M2", "est"]] - ret1[["M1", "est"]])
    expect_equal(ret1[["diff", "est"]],
                 sum(ret1[c("unit_marginal", "group_marginal", "interaction", "structural"), "est"]))
    expect_equal(ret1[["additions", "est"]], 0)
    expect_equal(ret1[["removals", "est"]], 0)

    # other way around
    ret <- mutual_difference(test_data1, test_data2, "g", "u", weight = "n", method = "ipf")

    expect_equal(ret[["diff", "est"]], ret[["M2", "est"]] - ret[["M1", "est"]])
    expect_equal(ret[["diff", "est"]],
                 sum(ret[c("unit_marginal", "group_marginal", "interaction", "structural"), "est"]))

    # same as mutual_total
    expect_equal(ret[["M1", "est"]], mutual_total(test_data1, "u", "g", weight = "n")[["M", "est"]])
    expect_equal(ret[["M2", "est"]], mutual_total(test_data2, "u", "g", weight = "n")[["M", "est"]])

    expect_equal(nrow(ret), 9)
    expect_equal(ncol(ret), 2)

    ret <- mutual_difference(test_data1, test_data2, "g", "u", weight = "n", method = "ipf", base = 2)
    expect_equal(ret[["diff", "est"]], ret[["M2", "est"]] - ret[["M1", "est"]])
    expect_equal(nrow(ret), 9)
    expect_equal(ncol(ret), 2)
})


test_that("mutual_difference MRC", {
    ret <- mutual_difference(test_data1, test_data2, "g", "u", weight = "n", method = "mrc")

    expect_equal(ret[["diff", "est"]], ret[["M2", "est"]] - ret[["M1", "est"]])
    expect_equal(ret[["diff", "est"]],
                 sum(ret[c("unit_marginal", "group_marginal", "structural"), "est"]))

    ret <- mutual_difference(test_data1, test_data2, "g", "u", weight = "n", method = "mrc")

    # other way around
    expect_equal(ret[["diff", "est"]], ret[["M2", "est"]] - ret[["M1", "est"]])
    expect_equal(ret[["diff", "est"]],
                 sum(ret[c("unit_marginal", "group_marginal", "structural"), "est"]))

    # same as mutual_total
    expect_equal(ret[["M1", "est"]], mutual_total(test_data1, "u", "g", weight = "n")[["M", "est"]])
    expect_equal(ret[["M2", "est"]], mutual_total(test_data2, "u", "g", weight = "n")[["M", "est"]])

    expect_equal(nrow(ret), 6)
    expect_equal(ncol(ret), 2)

    ret <- mutual_difference(test_data1, test_data2, "g", "u", weight = "n", method = "mrc", base = 2)
    expect_equal(ret[["diff", "est"]], ret[["M2", "est"]] - ret[["M1", "est"]])
    expect_equal(nrow(ret), 6)
    expect_equal(ncol(ret), 2)
})


test_that("mutual_difference MRC-ADJUSTED", {
    ret <- mutual_difference(test_data1, test_data2, "g", "u", weight = "n", method = "mrc_adjusted")

    expect_equal(ret[["diff", "est"]], ret[["M2", "est"]] - ret[["M1", "est"]])
    expect_equal(ret[["diff", "est"]],
                 sum(ret[c("unit_marginal", "group_marginal", "structural"), "est"]))
    expect_equal(ret[["additions", "est"]], 0)
    expect_equal(ret[["removals", "est"]], 0)

    # other way around
    ret <- mutual_difference(test_data1, test_data2, "g", "u", weight = "n", method = "mrc_adjusted")

    expect_equal(ret[["diff", "est"]], ret[["M2", "est"]] - ret[["M1", "est"]])
    expect_equal(ret[["diff", "est"]],
                 sum(ret[c("unit_marginal", "group_marginal", "structural"), "est"]))

    # same as mutual_total
    expect_equal(ret[["M1", "est"]], mutual_total(test_data1, "u", "g", weight = "n")[["M", "est"]])
    expect_equal(ret[["M2", "est"]], mutual_total(test_data2, "u", "g", weight = "n")[["M", "est"]])

    expect_equal(nrow(ret), 8)
    expect_equal(ncol(ret), 2)

    ret <- mutual_difference(test_data1, test_data2, "g", "u", weight = "n", method = "mrc_adjusted", base = 2)
    expect_equal(ret[["diff", "est"]], ret[["M2", "est"]] - ret[["M1", "est"]])
    expect_equal(nrow(ret), 8)
    expect_equal(ncol(ret), 2)
})


test_that("mutual_difference SE", {
    ret <- mutual_difference(test_data1, test_data2, "g", "u", weight = "n",
                            method = "ipf", se = TRUE, n_bootstrap = 5)
    expect_equal(nrow(ret), 9)
    expect_equal(ncol(ret), 3)
    expect_equal(all(ret[ret$est>0, "se"] > 0), TRUE)

    ret <- mutual_difference(test_data1, test_data2, "g", "u", weight = "n",
                            method = "mrc", se = TRUE, n_bootstrap = 5)
    expect_equal(nrow(ret), 6)
    expect_equal(ncol(ret), 3)
    expect_equal(all(ret$se > 0), TRUE)

    ret <- mutual_difference(test_data1, test_data2, "g", "u", weight = "n",
                            method = "mrc_adjusted", se = TRUE, n_bootstrap = 5)
    expect_equal(nrow(ret), 8)
    expect_equal(ncol(ret), 3)
    expect_equal(all(ret[ret$est>0, "se"] > 0), TRUE)
})

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

ipf <- mutual_difference(test_data1, test_data2, "g", "u", weight = "n", method = "ipf", precision = .1)
mrc <- mutual_difference(test_data1, test_data2, "g", "u", weight = "n", method = "mrc")
mrc_adj <- mutual_difference(test_data1, test_data2, "g", "u", weight = "n", method = "mrc_adjusted")
M1 <- mutual_total(test_data1, "g", "u", weight = "n")[["M", "est"]]
M2 <- mutual_total(test_data2, "g", "u", weight = "n")[["M", "est"]]

test_that("difference same as mutual_total (zero weights)", {    
    expect_equal(ipf[["M1", "est"]], M1)
    expect_equal(ipf[["M2", "est"]], M2)
    expect_equal(mrc[["M1", "est"]], M1)
    expect_equal(mrc[["M2", "est"]], M2)
    expect_equal(mrc_adj[["M1", "est"]], M1)
    expect_equal(mrc_adj[["M2", "est"]], M2)
})
