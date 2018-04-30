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

    within_wide <- mutual_within(test_data, "u", "g", within = "supergroup",
                                 weight = "n", wide = T)
    expect_equal(sum(within$p * within$M), sum(within_wide$p * within_wide$M))
    expect_equal(sum(within$h_weight * within$H), sum(within_wide$h_weight * within_wide$H))

    # H is between 0 and 1
    expect_equal(all(within$H >= 0 & within$H <= 1), TRUE)
    expect_equal(all(h >= 0 & h <= 1), TRUE)
})

test_that("option wide works", {
    nowide <- mutual_within(test_data, "u", "g", within = "supergroup", weight = "n")
    nowide_se <- mutual_within(test_data, "u", "g", within = "supergroup", weight = "n", se = T)
    wide <- mutual_within(test_data, "u", "g", within = "supergroup", weight = "n", wide = T)
    wide_se <- mutual_within(test_data, "u", "g", within = "supergroup", weight = "n", wide = T, se = T)

    expect_equal(ncol(nowide) + 1, ncol(nowide_se))
    expect_equal(nrow(nowide), 2*4)
    expect_equal(nrow(nowide), nrow(nowide_se))

    expect_equal(ncol(wide) + 4, ncol(wide_se))
    expect_equal(nrow(wide), 2)
    expect_equal(nrow(wide), nrow(wide_se))

    expect_equal(nowide[nowide$stat=="M",][["est"]], wide$M)
    expect_equal(nowide[nowide$stat=="p",][["est"]], wide$p)
    expect_equal(nowide[nowide$stat=="H",][["est"]], wide$H)
    expect_equal(nowide[nowide$stat=="h_weight",][["est"]], wide$h_weight)

    total <- mutual_total(test_data, "u", "g", within = "supergroup", weight = "n")
    expect_equal(total[["M", "est"]],
        sum(nowide[nowide$stat=="M", "est"] * nowide[nowide$stat=="p", "est"]))
    expect_equal(total[["M", "est"]], sum(wide$M * wide$p))
    expect_equal(total[["M", "est"]], sum(wide$H * wide$h_weight * entropy(test_data, "u", "n")))

    expect_equal(total[["H", "est"]],
        sum(nowide[nowide$stat=="H", "est"] * nowide[nowide$stat=="h_weight", "est"]))
    expect_equal(total[["H", "est"]], sum(wide$H * wide$h_weight))

    expect_equal(all(nowide_se['se'] > 0), TRUE)
})
