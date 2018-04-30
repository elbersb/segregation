library(segregation)
context("test_mutual_within")

test_data <- data.frame(
    u = c(rep("a", 4), rep("b", 4)),
    g = rep(c(1, 2, 3, 4), 2),
    supergroup = rep(c(12, 12, 34, 34), 2),
    n = c(40, 20, 5, 1, 20, 40, 60, 80),
    stringsAsFactors = FALSE
)

test_that("dimensions and bootstrapping", {
    within <- mutual_within(test_data, "u", "g",
                            within = "supergroup", weight = "n")
    expect_equal(dim(within), c(2*4, 3))

    within_se <- mutual_within(test_data, "u", "g",
                               within = "supergroup", weight = "n", se = TRUE)
    expect_equal(dim(within_se), c(2*4, 4))
})

test_that("between + within = total", {
    total <- mutual_total(test_data, "u", "g", within = "supergroup", weight = "n")
    m <- total[["M", "est"]]
    h <- total[["H", "est"]]

    within <- mutual_within(test_data, "u", "g", within = "supergroup", weight = "n")
    within <- unstack(within, form=est ~ stat)

    expect_equal(m, sum(within$p * within$M))
    expect_equal(h, sum(within$h_weight * within$H))

    # H is between 0 and 1
    expect_equal(all(within$H >= 0 & within$H <= 1), TRUE)
    expect_equal(all(h >= 0 & h <= 1), TRUE)
})
