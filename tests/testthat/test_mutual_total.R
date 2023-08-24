if (!identical(Sys.getenv("NOT_CRAN"), "true")) {
    return()
}

library("segregation")
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
        mutual_total(test_data, "u", "g", weight = "n")[stat == "M", est],
        mutual_total(test_data, "g", "u", weight = "n")[stat == "M", est]
    )

    expanded <- test_data[rep(seq_len(nrow(test_data)), test_data$n), 1:3]
    expect_equal(
        mutual_total(test_data, "u", "g", weight = "n")[stat == "M", est],
        mutual_total(expanded, "u", "g")[stat == "M", est]
    )

    expect_equal(
        mutual_total(expanded, "u", "g")[stat == "M", est],
        mutual_total(expanded, "g", "u")[stat == "M", est]
    )

    expect_equal(
        mutual_total(test_data, "u", "g", weight = "n", base = 2)[stat == "M", est],
        mutual_total(test_data, "g", "u", weight = "n", base = 2)[stat == "M", est]
    )

    expect_equal(
        mutual_total(test_data, "u", c("supergroup", "g"), weight = "n")[stat == "M", est],
        mutual_total(test_data, "u", "g", weight = "n")[stat == "M", est]
    )

    expect_equal(
        mutual_total(test_data, "u", c("supergroup", "g"), weight = "n")[stat == "M", est],
        mutual_total(test_data, c("supergroup", "g"), "u", weight = "n")[stat == "M", est]
    )
})

test_that("between + within = total", {
    expect_equal(
        mutual_total(test_data, "u", "g", weight = "n")[stat == "M", est],
        mutual_total(test_data, "u", "supergroup", weight = "n")[stat == "M", est] +
            mutual_total(test_data, "u", "g", within = "supergroup", weight = "n")[stat == "M", est]
    )
    expect_equal(
        mutual_total(test_data, "u", "g", weight = "n")[stat == "H", est],
        mutual_total(test_data, "u", "supergroup", weight = "n")[stat == "H", est] +
            mutual_total(test_data, "u", "g", within = "supergroup", weight = "n")[stat == "H", est]
    )
})

p_12 <- sum(test_data[test_data$supergroup == 12, "n"]) / sum(test_data$n)
p_34 <- sum(test_data[test_data$supergroup == 34, "n"]) / sum(test_data$n)
test_that("within estimations are correct", {
    d_12 <- test_data[test_data$supergroup == 12, ]
    d_34 <- test_data[test_data$supergroup == 34, ]
    expect_equal(
        p_12 * mutual_total(d_12, "u", "g", weight = "n")[stat == "M", est] +
            p_34 * mutual_total(d_34, "u", "g", weight = "n")[stat == "M", est],
        mutual_total(test_data, "u", "g", within = "supergroup", weight = "n")[stat == "M", est]
    )
    # this decomposition does not exist in the same way for H
})

test_that("H is correct", {
    ret <- mutual_total(test_data, "u", "g", weight = "n")
    expect_equal(ret[stat == "H", est] >= 0 & ret[stat == "H", est] <= 1, TRUE)
})

test_that("bootstrapping works", {
    ret <- mutual_total(test_data, "u", "g", weight = "n", se = TRUE, n_bootstrap = 10)
    expect_equal(dim(ret), c(2, 5))
    expect_equal(all(ret$se > 0), TRUE)

    ret <- mutual_total(test_data, "u", "g",
        weight = "n", se = TRUE, n_bootstrap = 10,
        within = "supergroup"
    )
    expect_equal(dim(ret), c(2, 5))
    expect_equal(all(ret$se > 0), TRUE)
})

test_that("bootstrap attributes exists", {
    ret <- mutual_total(test_data, "u", "g", weight = "n", se = TRUE, n_bootstrap = 10)
    expect_equal(dim(attr(ret, "bootstrap")), c(2 * 10, 2))
})

test_that("bootstrapping fails when sample size is non-integer", {
    test_data <- data.frame(
        u = c(rep("a", 4), rep("b", 4)),
        g = rep(c(1, 2, 3, 4), 2),
        n = c(40, 20, 5, 1.8, 20, 40, 60, 80),
        stringsAsFactors = FALSE
    )

    expect_error(mutual_total(test_data, "u", "g", weight = "n", se = TRUE, n_bootstrap = 10))
    # rescale
    test_data$n2 <- test_data$n / sum(test_data$n) * round(sum(test_data$n))
    ret <- mutual_total(test_data, "u", "g", weight = "n2", se = TRUE, n_bootstrap = 10)
    expect_equal(dim(ret), c(2, 5))
    expect_equal(all(ret$se > 0), TRUE)
})


test_data <- data.frame(
    u = c(rep("a", 4), rep("b", 4)),
    g = rep(c(1, 2, 3, 4), 2),
    n = c(40, 0, 0, 0, 0, 0, 0, 40)
)

test_that("zero weights no problem", {
    expect_equal(dim(mutual_total(test_data, "u", "g",
        weight = "n",
        se = TRUE, n_bootstrap = 10
    )), c(2, 5))
    expect_equal(dim(mutual_total(test_data, "u", "g", weight = "n")), c(2, 2))
    expect_equal(mutual_total(test_data, "u", "g", weight = "n")[stat == "M", est], log(2))
    expect_equal(mutual_total(test_data, "u", "g", weight = "n")[stat == "H", est], 1)

    test_data2 <- copy(test_data)
    test_data2$g <- as.factor(test_data2$g)
    expect_equal(
        mutual_total(test_data, "u", "g", weight = "n")[["est"]],
        mutual_total(test_data2, "u", "g", weight = "n")[["est"]]
    )
})

test_that("gives errors", {
    expect_error(mutual_total("test_data", "u", "g", weight = "n"), "not a data.frame")
    expect_error(
        mutual_total(test_data[test_data$u == "c", ], "u", "g", weight = "n"),
        "data.frame is empty"
    )

    expect_error(mutual_total(test_data, "u2", "g", weight = "n"), "u2 not in data.frame")
    expect_error(mutual_total(test_data, "u2", "g2", weight = "n"), "u2, g2 not in data.frame")
    expect_error(mutual_total(test_data, "u2", "g2", weight = "n2"), "u2, g2, n2 not in data.frame")

    test_data_constant1 <- data.frame(u = c(rep("a", 4), rep("b", 4)), g = 1)
    expect_error(mutual_total(test_data_constant1, "g", "u"), "group variable is constant")
    test_data_constant2 <- data.frame(g = c(rep("a", 4), rep("b", 4)), u = 1)
    expect_error(mutual_total(test_data_constant2, "g", "u"), "unit variable is constant")
})

test_that("debiasing works correctly", {
    nose <- mutual_total(test_data, "u", "g", weight = "n")
    withse <- mutual_total(test_data, "u", "g", weight = "n", se = TRUE)
    expect_equal(nose$est, withse$est + withse$bias)
})
