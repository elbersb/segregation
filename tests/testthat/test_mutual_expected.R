if (!identical(Sys.getenv("NOT_CRAN"), "true")) {
    return()
}

library("segregation")
context("test_mutual_expected")

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
    expect_equal(
        mutual_expected(data1, "u", "g", weight = "n")[stat == "M under 0", est],
        mutual_expected(data1, "g", "u", weight = "n")[stat == "M under 0", est],
        tolerance = 0.01
    )

    expect_equal(
        mutual_expected(data2, "u", "g", weight = "n")[stat == "M under 0", est],
        mutual_expected(data2, "g", "u", weight = "n")[stat == "M under 0", est],
        tolerance = 0.05
    )
})

test_that("fixed margins = FALSE", {
    expect_equal(
        mutual_expected(data1, "u", "g", weight = "n", fixed_margins = FALSE)[stat == "M under 0", est],
        mutual_expected(data1, "g", "u", weight = "n", fixed_margins = FALSE)[stat == "M under 0", est],
        tolerance = 0.01
    )
})

test_that("within argument", {
    within <- mutual_expected(school_ses, "ethnic_group", "school_id", within = "ses_quintile")
    # manually
    d <- data.table::as.data.table(school_ses)
    manually <- d[, mutual_expected(.SD, "ethnic_group", "school_id"), by = .(ses_quintile)]
    p <- d[, .(p = .N), by = .(ses_quintile)][, .(ses_quintile, p = p / sum(p))]
    manually <- merge(manually, p)
    manually <- manually[, .(est_manual = sum(est * p)), by = .(stat)]
    compare <- merge(within, manually)
    expect_equal(compare$est, compare$est_manual, tolerance = 0.01)
})


test_that("dissimilarity", {
    expect_error(dissimilarity_expected(data1, "u", "g", weight = "n"))
    expect_equal(dissimilarity_expected(data1, "g", "u", n_bootstrap = 500, weight = "n")$est,
        0.098,
        tolerance = 0.1
    )
    expect_equal(
        dissimilarity_expected(data1, "g", "u",
            n_bootstrap = 500, weight = "n",
            fixed_margins = FALSE
        )$est,
        0.1003,
        tolerance = 0.1
    )
})

test_that("dissimilarity - Winship 1977", {
    # see table 2
    mat <- matrix(c(rep(1, 1000), rep(9, 1000)), ncol = 2)
    d <- matrix_to_long(mat)
    expect_equal(
        dissimilarity_expected(d, "group", "unit", weight = "n")$est,
        0.387,
        tolerance = 0.1
    )

    mat <- matrix(c(rep(5, 1000), rep(5, 1000)), ncol = 2)
    d <- matrix_to_long(mat)
    expect_equal(
        dissimilarity_expected(d, "group", "unit", weight = "n")$est,
        0.246,
        tolerance = 0.1
    )

    mat <- matrix(c(rep(10, 1000), rep(90, 1000)), ncol = 2)
    d <- matrix_to_long(mat)
    expect_equal(
        dissimilarity_expected(d, "group", "unit", weight = "n")$est,
        0.131,
        tolerance = 0.1
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
        mutual_expected(dat, "g", "u", weight = "n"),
        "bootstrap with a total sample size"
    )
    expect_error(
        dissimilarity_expected(dat, "g", "u", weight = "n"),
        "bootstrap with a total sample size"
    )

    dat_within <- data.frame(
        u = c(rep("a", 4), rep("b", 4)),
        g = rep(c(1, 2), 4),
        supergroup = rep(c(12, 12, 34, 34), 2),
        n = c(40, 20, 5, 1, 20, 40, 60, 80),
        stringsAsFactors = FALSE
    )

    expect_message(mutual_expected(schools00, "race", "school",
        within = "district", weight = "n",
        n_bootstrap = 10
    ), "singleton items")
})
