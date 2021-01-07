library("segregation")
context("test_mutual_local")

test_data <- data.frame(
    u = c(rep("a", 4), rep("b", 4)),
    g = rep(c(1, 2, 3, 4), 2),
    n = c(40, 20, 5, 1, 20, 40, 60, 80)
)

local <- mutual_local(test_data, "u", "g", weight = "n")
local2 <- mutual_local(test_data, "g", "u", weight = "n")
localse <- mutual_local(test_data, "u", "g", weight = "n", se = TRUE, n_bootstrap = 10)
localbase2 <- mutual_local(test_data, "g", "u", weight = "n", base = 2)

test_that("local calculation works", {
    expect_equal(sum(local[stat == "p", est]), 1)
    expect_equal(sum(local2[local2$stat == "p", est]), 1)
    expect_equal(sum(localse[localse$stat == "p", est]), 1)

    expect_equal(
        sum(local[stat == "p", est] * local[stat == "ls", est]),
        sum(local2[local2$stat == "p", est] * local2[local2$stat == "ls", est])
    )
    expect_equal(
        sum(local[stat == "p", est] * local[stat == "ls", est]),
        mutual_total(test_data, "u", "g", weight = "n")[stat == "M", est]
    )
    expect_equal(
        sum(localbase2[stat == "p", est] * localbase2[stat == "ls", est]),
        mutual_total(test_data, "u", "g", weight = "n", base = 2)[stat == "M", est]
    )
})

test_that("return works", {
    expect_equal(nrow(local), 8)
    expect_equal(ncol(local), 3)
})

test_that("bootstrapping works", {
    expect_equal(nrow(localse), 8)
    expect_equal(ncol(localse), 5)
})

test_that("bootstrapping fails when sample size is non-integer", {
    test_data <- data.frame(
        u = c(rep("a", 4), rep("b", 4)),
        g = rep(c(1, 2, 3, 4), 2),
        n = c(40, 20, 5, 1, 20, 40, 60, 80.3)
    )

    expect_error(mutual_local(test_data, "u", "g", weight = "n", se = TRUE))
    # rescale
    test_data$n2 <- test_data$n / sum(test_data$n) * round(sum(test_data$n))
    ret <- mutual_local(test_data, "u", "g", weight = "n2", se = TRUE)
    expect_equal(all(ret$se > 0), TRUE)
})

test_that("option wide works", {
    nowide <- mutual_local(test_data, "u", "g", weight = "n")
    nowide_se <- mutual_local(test_data, "u", "g", weight = "n", se = TRUE)
    wide <- mutual_local(test_data, "u", "g", weight = "n", wide = TRUE)
    wide_se <- mutual_local(test_data, "u", "g", weight = "n", wide = TRUE, se = TRUE)

    expect_equal(ncol(nowide) + 2, ncol(nowide_se))
    expect_equal(nrow(nowide), 8)
    expect_equal(nrow(nowide), nrow(nowide_se))
    expect_equal(nrow(nowide), nrow(wide) * 2)
    expect_equal(ncol(wide) + 4, ncol(wide_se))
    expect_equal(nrow(wide), nrow(wide_se))

    expect_equal(nowide[stat == "ls", est], wide$ls)
    expect_equal(nowide[stat == "p", est], wide$p)

    total <- mutual_total(test_data, "u", "g", weight = "n")
    expect_equal(total[stat == "M", est],
                 sum(nowide[stat == "ls", est] * nowide[stat == "p", "est"]))
    expect_equal(total[stat == "M", est], sum(wide$ls * wide$p))
    expect_equal(total[stat == "H", est], sum(wide$ls * wide$p) / entropy(test_data, "u", "n"))

    expect_equal(all(nowide_se[["se"]] > 0), TRUE)
})
