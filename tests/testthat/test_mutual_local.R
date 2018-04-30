library(segregation)
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
    expect_equal(sum(local[local$stat=="p", "est"]), 1)
    expect_equal(sum(local2[local2$stat=="p", "est"]), 1)
    expect_equal(sum(localse[localse$stat=="p", "est"]), 1)

    expect_equal(
        sum(local[local$stat=="M_unit", "est"]),
        sum(local2[local2$stat=="M_unit", "est"])
    )
    expect_equal(
        sum(local[local$stat=="M_unit", "est"]),
        mutual_total(test_data, "u", "g", weight = "n")[["M", "est"]]
    )
    expect_equal(
        sum(localbase2[localbase2$stat=="M_unit", "est"]),
        mutual_total(test_data, "u", "g", weight = "n", base = 2)[["M", "est"]]
    )
})

test_that("return works", {
    expect_equal(nrow(local), 4*3)
    expect_equal(ncol(local), 3)
})

test_that("bootstrapping works", {
    expect_equal(nrow(localse), 4*3)
    expect_equal(ncol(localse), 4)
})
