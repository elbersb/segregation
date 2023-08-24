if (!identical(Sys.getenv("NOT_CRAN"), "true")) {
    return()
}

library("segregation")
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
        within = "supergroup", weight = "n"
    )
    expect_equal(dim(within), c(2 * 4, 3))

    within_se <- mutual_within(test_data, "u", "g",
        within = "supergroup", weight = "n", se = TRUE, n_bootstrap = 10
    )
    expect_equal(dim(within_se), c(2 * 4, 6))
})

test_that("bootstrap attributes exists", {
    within_se <- mutual_within(test_data, "u", "g",
        within = "supergroup", weight = "n", se = TRUE, n_bootstrap = 10
    )

    expect_equal(dim(attr(within_se, "bootstrap")), c(10 * length(unique(test_data$supergroup)) * 4, 3))
})

test_that("bootstrapping fails when sample size is non-integer", {
    test_data <- data.frame(
        u = c(rep("a", 4), rep("b", 4)),
        g = rep(c(1, 2, 3, 4), 2),
        supergroup = rep(c(12, 12, 34, 34), 2),
        n = c(40.7, 20, 5, 1, 20.5, 40, 60, 80),
        stringsAsFactors = FALSE
    )

    expect_error(mutual_within(test_data, "u", "g",
        within = "supergroup", weight = "n", se = TRUE, n_bootstrap = 10
    ))
    # rescale
    test_data$n2 <- test_data$n / sum(test_data$n) * round(sum(test_data$n))
    ret <- mutual_within(test_data, "u", "g",
        within = "supergroup", weight = "n2", se = TRUE, n_bootstrap = 10
    )
    expect_equal(dim(ret), c(2 * 4, 6))
})

test_that("between + within = total", {
    total <- mutual_total(test_data, "u", "g", within = "supergroup", weight = "n")
    m <- total[stat == "M", est]
    h <- total[stat == "H", est]

    within <- mutual_within(test_data, "u", "g", within = "supergroup", weight = "n")
    within <- unstack(within, form = est ~ stat)

    expect_equal(m, sum(within$p * within$M))
    expect_equal(h, sum(within$p * within$ent_ratio * within$H))

    within_wide <- mutual_within(test_data, "u", "g",
        within = "supergroup",
        weight = "n", wide = T
    )
    expect_equal(sum(within$p * within$M), sum(within_wide$p * within_wide$M))
    expect_equal(
        sum(within$p * within$ent_ratio * within$H),
        sum(within_wide$p * within_wide$ent_ratio * within_wide$H)
    )

    # H is between 0 and 1
    expect_equal(all(within$H >= 0 & within$H <= 1), TRUE)
    expect_equal(all(h >= 0 & h <= 1), TRUE)
})

test_that("option wide works", {
    nowide <- mutual_within(test_data, "u", "g",
        within = "supergroup", weight = "n"
    )
    nowide_se <- mutual_within(test_data, "u", "g",
        within = "supergroup", weight = "n", se = TRUE, n_bootstrap = 10
    )
    wide <- mutual_within(test_data, "u", "g",
        within = "supergroup", weight = "n", wide = TRUE
    )
    wide_se <- mutual_within(test_data, "u", "g",
        within = "supergroup", weight = "n", wide = TRUE, se = TRUE, n_bootstrap = 10
    )

    expect_equal(ncol(nowide) + 3, ncol(nowide_se))
    expect_equal(nrow(nowide), 2 * 4)
    expect_equal(nrow(nowide), nrow(nowide_se))

    expect_equal(ncol(wide) + 3 * 4, ncol(wide_se))
    expect_equal(nrow(wide), 2)
    expect_equal(nrow(wide), nrow(wide_se))

    expect_equal(nowide[stat == "M", est], wide$M)
    expect_equal(nowide[stat == "p", est], wide$p)
    expect_equal(nowide[stat == "H", est], wide$H)
    expect_equal(nowide[stat == "ent_ratio", est], wide$ent_ratio)

    total <- mutual_total(test_data, "u", "g", within = "supergroup", weight = "n")
    expect_equal(
        total[stat == "M", est],
        sum(nowide[stat == "M", est] * nowide[stat == "p", est])
    )
    expect_equal(total[stat == "M", est], sum(wide$M * wide$p))
    expect_equal(
        total[stat == "M", est],
        sum(wide$H * wide$p * wide$ent_ratio * entropy(test_data, "u", "n"))
    )

    expect_equal(
        total[stat == "H", est],
        sum(nowide[stat == "H", est] *
            nowide[stat == "p", est] *
            nowide[stat == "ent_ratio", est])
    )
    expect_equal(total[stat == "H", est], sum(wide$H * wide$p * wide$ent_ratio))

    expect_equal(all(nowide_se[["se"]] > 0), TRUE)
})
