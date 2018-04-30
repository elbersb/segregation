library(segregation)
context("test_entropy")

test_that("within estimations is correct", {
    expect_equal(entropy(data.frame(x = c(1)), "x"), 0)
    expect_equal(entropy(data.frame(x = c(1, 2)), "x"), log(2))
    expect_equal(entropy(data.frame(x = c(1, 2, 3)), "x"), log(3))

    expect_equal(entropy(data.frame(x = c(1, 2), n = c(10, 10)), "x", weight = "n"),
                 log(2))
    expect_equal(entropy(data.frame(x = c(1, 2), n = c(10, 10)), "x", weight = "n", base = 2),
                 1)
    expect_equal(entropy(data.frame(x = c(1, 2), n = c(10, 30)), "x", weight = "n"),
                 .25*log(1/.25)+.75*log(1/.75))
})