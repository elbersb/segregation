library(segregation)
context("test_mutual_total")

test_data <- data.frame(
    u = c(rep("a", 4), rep("b", 4)),
    g = rep(c(1, 2, 3, 4), 2),
    supergroup = rep(c(12, 12, 34, 34), 2),
    n = c(40, 20, 5, 1, 20, 40, 60, 80),
    stringsAsFactors = FALSE
)

test_that("mutual M works both ways around", {
    expect_equal(
        mutual_total(test_data, "u", "g", weight = "n")[["M", "est"]],
        mutual_total(test_data, "g", "u", weight = "n")[["M", "est"]]
    )

    expanded <- test_data[rep(seq_len(nrow(test_data)), test_data$n), 1:3]
    expect_equal(
        mutual_total(test_data, "u", "g", weight = "n")[["M", "est"]],
        mutual_total(expanded, "u", "g")[["M", "est"]],
        mutual_total(expanded, "g", "u")[["M", "est"]]
    )

    expect_equal(
        mutual_total(test_data, "u", "g", weight = "n", base = 2)[["M", "est"]],
        mutual_total(test_data, "g", "u", weight = "n", base = 2)[["M", "est"]]
    )

    expect_equal(
        mutual_total(test_data, "u", c("supergroup", "g"), weight = "n")[["M", "est"]],
        mutual_total(test_data, "u", "g", weight = "n")[["M", "est"]]
    )

    expect_equal(
        mutual_total(test_data, "u", c("supergroup", "g"), weight = "n")[["M", "est"]],
        mutual_total(test_data, c("supergroup", "g"), "u", weight = "n")[["M", "est"]]
    )
})

test_that("as tibble works", {
    ret <- mutual_total(test_data, "u", "g", weight = "n")
    expect_true("data.frame" %in% class(ret))

    if(requireNamespace("tibble", quietly = TRUE)) {
        library(tibble)
        ret <- mutual_total(test_data, "u", "g", weight = "n")
        expect_true("tbl_df" %in% class(ret))
    }
})

test_that("between + within = total", {
    expect_equal(
        mutual_total(test_data, "u", "g", weight = "n")[["M", "est"]],
        mutual_total(test_data, "u", "supergroup", weight = "n")[["M", "est"]] +
            mutual_total(test_data, "u", "g", within = "supergroup", weight = "n")[["M", "est"]]
    )
    expect_equal(
        mutual_total(test_data, "u", "g", weight = "n")[["H", "est"]],
        mutual_total(test_data, "u", "supergroup", weight = "n")[["H", "est"]] +
            mutual_total(test_data, "u", "g", within = "supergroup", weight = "n")[["H", "est"]]
    )
})

p_12 <- sum(test_data[test_data$supergroup == 12, "n"]) / sum(test_data$n)
p_34 <- sum(test_data[test_data$supergroup == 34, "n"]) / sum(test_data$n)
test_that("within estimations are correct", {
    expect_equal(
        p_12 * mutual_total(test_data[test_data$supergroup == 12, ], "u", "g", weight = "n")[["M", "est"]] +
            p_34 * mutual_total(test_data[test_data$supergroup == 34, ], "u", "g", weight = "n")[["M", "est"]],
        mutual_total(test_data, "u", "g", within = "supergroup", weight = "n")[["M", "est"]]
    )
    # this decomposition does not exist in the same way for H
})

test_that("H is correct", {
    ret <- mutual_total(test_data, "u", "g", weight = "n")
    expect_equal(ret[["H", "est"]] >= 0 & ret[["H", "est"]] <= 1, TRUE)
})

test_that("bootstrapping works", {
    ret <- mutual_total(test_data, "u", "g", weight = "n", se = TRUE)
    expect_equal(dim(ret), c(2, 3))
    expect_equal(all(ret$se > 0), TRUE)

    ret <- mutual_total(test_data, "u", "g", weight = "n", se = TRUE,
                        within = "supergroup")
    expect_equal(dim(ret), c(2, 3))
    expect_equal(all(ret$se > 0), TRUE)
})

test_data <- data.frame(
    u = c(rep("a", 4), rep("b", 4)),
    g = rep(c(1, 2, 3, 4), 2),
    n = c(40, 0, 0, 0, 0, 0, 0, 40)
)

test_that("zero weights no problem", {
    expect_equal(dim(mutual_total(test_data, "u", "g", weight = "n", se = TRUE)), c(2, 3))
    expect_equal(dim(mutual_total(test_data, "u", "g", weight = "n")), c(2, 2))
    expect_equal(mutual_total(test_data, "u", "g", weight = "n")[["M", "est"]], log(2))
    expect_equal(mutual_total(test_data, "u", "g", weight = "n")[["H", "est"]], 1)

    test_data2 <- copy(test_data)
    test_data2$g <- as.factor(test_data2$g)
    expect_equal(
        mutual_total(test_data, "u", "g", weight = "n")[["est"]],
        mutual_total(test_data2, "u", "g", weight = "n")[["est"]])
})



