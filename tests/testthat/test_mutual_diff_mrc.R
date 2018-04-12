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

test_that("mutual_difference", {
    ret = mutual_difference(test_data1, test_data2, "u", "g", weight = "n", method = "mrc")

    expect_equal(ret[["diff", "est"]], ret[["M2", "est"]] - ret[["M1", "est"]])
    expect_equal(ret[["diff", "est"]],
                 ret[["unit_entropy", "est"]] + ret[["group_marginal", "est"]] + ret[["invariant", "est"]])

    ret = mutual_difference(test_data1, test_data2, "g", "u", weight = "n", method = "mrc")

    expect_equal(ret[["diff", "est"]], ret[["M2", "est"]] - ret[["M1", "est"]])
    expect_equal(ret[["diff", "est"]],
                 ret[["unit_entropy", "est"]] + ret[["group_marginal", "est"]] + ret[["invariant", "est"]])

    expect_equal(nrow(ret), 6)
    expect_equal(ncol(ret), 2)
})

test_that("mutual_difference SE", {
    ret = mutual_difference(test_data1, test_data2, "u", "g", weight = "n", method = "mrc", se = TRUE)

    expect_equal(nrow(ret), 6)
    expect_equal(ncol(ret), 3)
})

test_that("mutual_mrc same as mutual_total", {
    expect_equal(
        mutual_difference(test_data1, test_data2, "u", "g", weight = "n", method = "mrc")[["M1", "est"]],
        mutual_total(test_data1, "u", "g", weight = "n")[["M", "est"]]
    )
    expect_equal(
        mutual_difference(test_data1, test_data2, "u", "g", weight = "n", method = "mrc")[["M2", "est"]],
        mutual_total(test_data2, "u", "g", weight = "n")[["M", "est"]]
    )
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

test_that("mutual_mrc same as mutual_total (zero weights)", {
    expect_equal(
        mutual_difference(test_data1, test_data2, "u", "g", weight = "n", method = "mrc")[["M1", "est"]],
        mutual_total(test_data1, "u", "g", weight = "n")[["M", "est"]]
    )
    expect_equal(
        mutual_difference(test_data1, test_data2, "u", "g", weight = "n", method = "mrc")[["M2", "est"]],
        mutual_total(test_data2, "u", "g", weight = "n")[["M", "est"]]
    )
})

test_that("mutual_mrc same both ways", {
    expect_equal(
        mutual_difference(test_data1, test_data2, "u", "g", weight = "n", method = "mrc")[["diff", "est"]],
        mutual_difference(test_data1, test_data2, "g", "u", weight = "n", method = "mrc")[["diff", "est"]]
    )
    expect_equal(
        mutual_difference(test_data1, test_data2, "u", "g", weight = "n", method = "mrc")[["M1", "est"]],
        mutual_difference(test_data1, test_data2, "g", "u", weight = "n", method = "mrc")[["M1", "est"]]
    )
    expect_equal(
        mutual_difference(test_data2, test_data1, "u", "g", weight = "n", method = "mrc")[["M1", "est"]],
        mutual_difference(test_data1, test_data2, "g", "u", weight = "n", method = "mrc")[["M2", "est"]]
    )
})

test_that("mutual_mrc empty cells", {
    ret = mutual_difference(test_data1, test_data2, "u", "g", weight = "n", method = "mrc")

    expect_equal(ret[["diff", "est"]], ret[["M2", "est"]] - ret[["M1", "est"]])
    expect_equal(ret[["diff", "est"]],
                 ret[["unit_entropy", "est"]] + ret[["group_marginal", "est"]] + ret[["invariant", "est"]])

    ret = mutual_difference(test_data1, test_data2, "g", "u", weight = "n", method = "mrc")

    expect_equal(ret[["diff", "est"]], ret[["M2", "est"]] - ret[["M1", "est"]])
    expect_equal(ret[["diff", "est"]],
                 ret[["unit_entropy", "est"]] + ret[["group_marginal", "est"]] + ret[["invariant", "est"]])

    expect_equal(nrow(ret), 6)
    expect_equal(ncol(ret), 2)
})
