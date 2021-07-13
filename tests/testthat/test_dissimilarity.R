library("segregation")
context("test_dissimilarity")

test_that("correct calculations", {
    m0 <- matrix_to_long(matrix(c(100, 100, 100, 100, 100, 100), ncol = 2))
    expect_equal(dissimilarity(m0, "group", "unit", weight = "n")$est[[1]], 0)

    m1 <- matrix_to_long(matrix(c(100, 60, 40, 0, 0, 40, 60, 100), ncol = 2))
    m2 <- matrix_to_long(matrix(c(80, 80, 20, 20, 20, 20, 80, 80), ncol = 2))
    expect_equal(dissimilarity(m1, "group", "unit", weight = "n")$est[[1]], .6)
    expect_equal(dissimilarity(m2, "group", "unit", weight = "n")$est[[1]], .6)

    m3 <- matrix_to_long(matrix(c(100, 100, 0, 0, 0, 0, 100, 100), ncol = 2))
    expect_equal(dissimilarity(m3, "group", "unit", weight = "n")$est[[1]], 1)
})

test_that("SE works", {
    m0 <- matrix_to_long(matrix(c(100, 60, 40, 0, 0, 40, 60, 100), ncol = 2))
    d <- dissimilarity(m0, "group", "unit", weight = "n", se = TRUE)
    expect_equal(dim(d), c(1, 5))
    expect_equal(d$se > 0, TRUE)
    expect_equal(dim(attr(d, "bootstrap")), c(100, 2))
})

test_that("names of columns", {
    m0 <- matrix_to_long(matrix(c(100, 60, 40, 0, 0, 40, 60, 100), ncol = 2),
                         group = "race", unit = "tract")

    d <- dissimilarity(m0, "race", "tract", weight = "n")
    expect_equal(dim(d), c(1, 2))

    data.table::setDT(m0)
    d <- dissimilarity(m0, "race", "tract", weight = "n")
    expect_equal(dim(d), c(1, 2))
})


test_that("bootstrapping fails when sample size is non-integer", {
    m0 <- matrix_to_long(matrix(c(100.3, 60, 40, 0, 0, 40, 60, 100), ncol = 2))
    expect_error(dissimilarity(m0, "group", "unit", weight = "n", se = TRUE))
})

test_that("gives error when group > 2", {
    m0 <- matrix_to_long(matrix(c(100, 60, 40, 10, 20, 40, 60, 100, 50), ncol = 3))
    expect_error(dissimilarity(m0, "group", "unit", weight = "n"))
})

