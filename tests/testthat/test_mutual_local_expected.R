if (!identical(Sys.getenv("NOT_CRAN"), "true")) {
    return()
}

library("segregation")
context("test_mutual_local_expected")

data1 <- data.frame(
    u = rep(c(1, 2, 3, 4), 2),
    g = c(rep("a", 4), rep("b", 4)),
    n = c(40, 20, 5, 1, 20, 40, 60, 80),
    stringsAsFactors = FALSE
)

data2 <- data.frame(
    u = c(1:10, 1:10),
    g = c(rep("a", 10), rep("b", 10)),
    n = c(rep(1, 10), rep(9, 10)),
    stringsAsFactors = FALSE
)

test_that("works both ways around", {
    data1a <- mutual_local_expected(data1, "u", "g", weight = "n")
    data1b <- mutual_local_expected(data1, "g", "u", weight = "n")
    expect_equal(nrow(data1a), 2)
    expect_equal(nrow(data1b), 4)
    expect_equal(data1a[, sum(est * p_mean)], data1b[, sum(est * p_mean)], tolerance = 0.01)

    data2a <- mutual_local_expected(data2, "u", "g", weight = "n")
    data2b <- mutual_local_expected(data2, "g", "u", weight = "n")
    expect_equal(nrow(data2a), 2)
    expect_equal(nrow(data2b), 10)
    expect_equal(data2a[, sum(est * p_mean)], data2b[, sum(est * p_mean)], tolerance = 0.01)
})

test_that("works nested", {
    data1$superunit <- data1$u <= 3
    a <- mutual_local_expected(data1, "g", c("superunit", "u"), weight = "n")
    b <- mutual_local_expected(data1, "g", "u", weight = "n")

    expect_equal(a$est, b$est, tolerance = 0.01)
    expect_equal(a$se, b$se, tolerance = 0.01)
    expect_equal(length(a$est), 4)
})

test_that("fixed margins = FALSE", {
    data1a <- mutual_local_expected(data1, "u", "g", weight = "n", fixed_margins = FALSE)
    data1b <- mutual_local_expected(data1, "g", "u", weight = "n", fixed_margins = FALSE)
    expect_equal(nrow(data1a), 2)
    expect_equal(nrow(data1b), 4)
    expect_equal(data1a[, sum(est * p_mean)], data1b[, sum(est * p_mean)], tolerance = 0.01)

    data2a <- mutual_local_expected(data2, "u", "g", weight = "n", fixed_margins = FALSE)
    data2b <- mutual_local_expected(data2, "g", "u", weight = "n", fixed_margins = FALSE)
    expect_equal(nrow(data2a), 2)
    expect_equal(nrow(data2b), 10)
    expect_equal(data2a[, sum(est * p_mean)], data2b[, sum(est * p_mean)], tolerance = 0.01)
})

test_that("aligns with mutual_total_expected", {
    expect_equal(
        mutual_local_expected(data1, "u", "g", weight = "n")[, sum(est * p_mean)],
        mutual_total_expected(data1, "u", "g", weight = "n")[stat == "M under 0", est],
        tolerance = 0.01
    )

    expect_equal(
        mutual_local_expected(data2, "u", "g", weight = "n")[, sum(est * p_mean)],
        mutual_total_expected(data2, "u", "g", weight = "n")[stat == "M under 0", est],
        tolerance = 0.01
    )

    expect_equal(
        mutual_local_expected(data1, "u", "g", weight = "n", fixed_margins = FALSE)[, sum(est * p_mean)],
        mutual_total_expected(data1, "u", "g", weight = "n", fixed_margins = FALSE)[stat == "M under 0", est],
        tolerance = 0.01
    )

    expect_equal(
        mutual_local_expected(data2, "u", "g", weight = "n", fixed_margins = FALSE)[, sum(est * p_mean)],
        mutual_total_expected(data2, "u", "g", weight = "n", fixed_margins = FALSE)[stat == "M under 0", est],
        tolerance = 0.01
    )
})


test_that("errors", {
    dat <- data.frame(
        u = rep(c(1, 2, 3, 4), 2),
        g = c(rep("a", 4), rep("b", 4)),
        n = c(40.2, 20, 5, 1, 20, 40, 60, 80),
        stringsAsFactors = FALSE
    )

    expect_error(
        mutual_local_expected(dat, "g", "u", weight = "n"),
        "bootstrap with a total sample size"
    )
})
