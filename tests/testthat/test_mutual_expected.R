library("segregation")
context("test_mutual_expected")

data1 <- data.frame(
    u = rep(c(1, 2, 3, 4), 2),
    g = c(rep("a", 4), rep("b", 4)),
    n = c(40, 20, 5, 1, 20, 40, 60, 80),
    stringsAsFactors = FALSE)

data2 <- data.frame(
    u = c(1:10, 1:10),
    g = c(rep("a", 10), rep("b", 10)),
    n = c(rep(1, 10), rep(9, 10)),
    stringsAsFactors = FALSE)

test_that("works both ways around", {
    expect_equal(
        mutual_expected(data1, "u", "g", weight = "n")[stat == "M under 0", est],
        mutual_expected(data1, "g", "u", weight = "n")[stat == "M under 0", est],
            tolerance = 0.01)

    expect_equal(
        mutual_expected(data2, "u", "g", weight = "n")[stat == "M under 0", est],
        mutual_expected(data2, "g", "u", weight = "n")[stat == "M under 0", est],
            tolerance = 0.05)
})

test_that("fixed margins = FALSE", {
    expect_equal(
        mutual_expected(data1, "u", "g", weight = "n", fixed_margins = FALSE)[stat == "M under 0", est],
        mutual_expected(data1, "g", "u", weight = "n", fixed_margins = FALSE)[stat == "M under 0", est],
        tolerance = 0.01)
})
