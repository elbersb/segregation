library(segregation)
context("test_mutual_total")

test_data <- data.frame(
    u = c(rep("a", 4), rep("b", 4)),
    g = rep(c(1, 2, 3, 4), 2),
    supergroup = rep(c(12, 12, 34, 34), 2),
    n = c(40, 20, 5, 1, 20, 40, 60, 80)
)

test_that("mutual works both ways around", {
    expect_equal(
        mutual_total(test_data, "u", "g", weight = "n")[["M", "est"]],
        mutual_total(test_data, "g", "u", weight = "n")[["M", "est"]]
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

test_that("between + within = total", {
    expect_equal(
        mutual_total(test_data, "u", "g", weight = "n")[["M", "est"]],
        mutual_total(test_data, "u", "supergroup", weight = "n")[["M", "est"]] +
            mutual_total(test_data, "u", "g", within = "supergroup", weight = "n")[["M", "est"]]
    )
})

p_12 <- sum(test_data[test_data$supergroup == 12, "n"]) / sum(test_data$n)
p_34 <- sum(test_data[test_data$supergroup == 34, "n"]) / sum(test_data$n)
test_that("within estimations is correct", {
    expect_equal(
        p_12 * mutual_total(test_data[test_data$supergroup == 12, ], "u", "g", weight = "n")[["M", "est"]] +
            p_34 * mutual_total(test_data[test_data$supergroup == 34, ], "u", "g", weight = "n")[["M", "est"]],
        mutual_total(test_data, "u", "g", within = "supergroup", weight = "n")[["M", "est"]]
    )
})

test_that("bootstrapping works", {
    ret = mutual_total(test_data, "u", "g", weight = "n", se = TRUE, n_bootstrap = 20)
    expect_equal(dim(ret), c(3, 3))
})

test_data <- data.frame(
    u = c(rep("a", 4), rep("b", 4)),
    g = rep(c(1, 2, 3, 4), 2),
    n = c(40, 0, 0, 0, 0, 0, 0, 40)
)

test_that("zero weights no problem", {
    expect_equal(dim(mutual_total(test_data, "u", "g", weight = "n", se = TRUE)), c(3, 3))
    expect_equal(dim(mutual_total(test_data, "u", "g", weight = "n")), c(3, 2))
})

test_that("max bound correct", {
    expect_equal(mutual_total(test_data, "u", "g", weight = "n")[["M", "est"]], log(2))
    expect_equal(mutual_total(test_data, "u", "g", weight = "n")[["M_max", "est"]], log(2))
})
