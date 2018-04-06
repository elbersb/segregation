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

test_that("mutual_diff_mrc", {
    ret = mutual_diff_mrc(test_data1, test_data2, "u", "g", weight = "n")

    expect_equal(ret$diff, ret$M2 - ret$M1)
    expect_equal(ret$diff, sum(ret$unit_entropy, ret$group_dist, ret$invariant))

    ret = mutual_diff_mrc(test_data1, test_data2, "g", "u", weight = "n")

    expect_equal(ret$diff, ret$M2 - ret$M1)
    expect_equal(ret$diff, sum(ret$unit_entropy, ret$group_dist, ret$invariant))

    expect_length(ret, 6)
    expect_length(unlist(ret), 6)
})

test_that("mutual_diff_mrc SE", {
    ret = mutual_diff_mrc(test_data1, test_data2, "u", "g", weight = "n", se=TRUE)

    expect_length(ret, 6)
    expect_length(unlist(ret), 12)
})

test_that("mutual_mrc same as mutual_total", {
    expect_equal(
        mutual_diff_mrc(test_data1, test_data2, "u", "g", weight = "n")$M1,
        mutual_total(test_data1, "u", "g", weight = "n")$M
    )
    expect_equal(
        mutual_diff_mrc(test_data1, test_data2, "u", "g", weight = "n")$M2,
        mutual_total(test_data2, "u", "g", weight = "n")$M
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
        mutual_diff_mrc(test_data1, test_data2, "u", "g", weight = "n")$M1,
        mutual_total(test_data1, "u", "g", weight = "n")$M
    )
    expect_equal(
        mutual_diff_mrc(test_data1, test_data2, "u", "g", weight = "n")$M2,
        mutual_total(test_data2, "u", "g", weight = "n")$M
    )
})

test_that("mutual_mrc same both ways", {
    expect_equal(
        mutual_diff_mrc(test_data1, test_data2, "u", "g", weight = "n")$diff,
        mutual_diff_mrc(test_data1, test_data2, "g", "u", weight = "n")$diff
    )
    expect_equal(
        mutual_diff_mrc(test_data1, test_data2, "u", "g", weight = "n")$M1,
        mutual_diff_mrc(test_data1, test_data2, "g", "u", weight = "n")$M1
    )
    expect_equal(
        mutual_diff_mrc(test_data2, test_data1, "u", "g", weight = "n")$M1,
        mutual_diff_mrc(test_data1, test_data2, "g", "u", weight = "n")$M2
    )
})

test_that("mutual_mrc empty cells", {
    ret = mutual_diff_mrc(test_data1, test_data2, "u", "g", weight = "n")

    expect_equal(ret$diff, ret$M2 - ret$M1)
    expect_equal(ret$diff, sum(ret$unit_entropy, ret$group_dist, ret$invariant))

    ret = mutual_diff_mrc(test_data1, test_data2, "g", "u", weight = "n")

    expect_equal(ret$diff, ret$M2 - ret$M1)
    expect_equal(ret$diff, sum(ret$unit_entropy, ret$group_dist, ret$invariant))

    expect_length(ret, 6)
    expect_length(unlist(ret), 6)
})
