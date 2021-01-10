library("segregation")
context("test_matrix_to_long")

test_that("accept only matrix", {
    a <- data.frame()
    expect_error(matrix_to_long(a))
})

test_that("no names", {
    m = matrix(c(10, 20, 30, 30, 20, 10), nrow = 3)
    long = matrix_to_long(m)
    expect_equal(names(long), c("unit", "group", "n"))
    expect_equal(long$unit, as.character(rep(1:3, 2)))
    expect_equal(long$group, as.character(c(1, 1, 1, 2, 2, 2)))
})

test_that("rownames only", {
    m = matrix(c(10, 20, 30, 30, 20, 10), nrow = 3)
    colnames(m) <- c("A", "B")
    long = matrix_to_long(m)
    expect_equal(names(long), c("unit", "group", "n"))
    expect_equal(long$unit, as.character(rep(1:3, 2)))
    expect_equal(long$group, as.character(c("A", "A", "A", "B", "B", "B")))
})

test_that("colnames only", {
    m = matrix(c(10, 20, 30, 30, 20, 10), nrow = 3)
    rownames(m) <- c("S1", "S2", "S3")
    long = matrix_to_long(m)
    expect_equal(names(long), c("unit", "group", "n"))
    expect_equal(long$unit, rep(c("S1", "S2", "S3"), 2))
    expect_equal(long$group, as.character(c(1, 1, 1, 2, 2, 2)))
})

test_that("rownames + colnames", {
    m = matrix(c(10, 20, 30, 30, 20, 10), nrow = 3)
    colnames(m) <- c("A", "B")
    rownames(m) <- c("S1", "S2", "S3")
    long = matrix_to_long(m)
    expect_equal(names(long), c("unit", "group", "n"))
    expect_equal(long$unit, rep(c("S1", "S2", "S3"), 2))
    expect_equal(long$group, as.character(c("A", "A", "A", "B", "B", "B")))
})

test_that("arguments", {
    # drop zero
    m = matrix(c(10, 20, 30, 0, 20, 0), nrow = 3)
    long1 = matrix_to_long(m)
    expect_equal(nrow(long1), 4)
    long2 = matrix_to_long(m, drop_zero = FALSE)
    expect_equal(nrow(long2), 6)

    # change names
    long3 = matrix_to_long(m, "race", "school", weight = "weight")
    expect_equal(names(long3), c("school", "race", "weight"))
})


