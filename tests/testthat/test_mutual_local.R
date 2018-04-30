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
        sum(local[local$stat=="p", "est"] * local[local$stat=="ls", "est"]),
        sum(local2[local2$stat=="p", "est"] * local2[local2$stat=="ls", "est"])
    )
    expect_equal(
        sum(local[local$stat=="p", "est"] * local[local$stat=="ls", "est"]),
        mutual_total(test_data, "u", "g", weight = "n")[["M", "est"]]
    )
    expect_equal(
        sum(localbase2[localbase2$stat=="p", "est"] * localbase2[localbase2$stat=="ls", "est"]),
        mutual_total(test_data, "u", "g", weight = "n", base = 2)[["M", "est"]]
    )
})

test_that("return works", {
    expect_equal(nrow(local), 2*4)
    expect_equal(ncol(local), 3)
})

test_that("bootstrapping works", {
    expect_equal(nrow(localse), 2*4)
    expect_equal(ncol(localse), 4)
})

test_that("option wide works", {
    nowide <- mutual_local(test_data, "u", "g", weight = "n")
    nowide_se <- mutual_local(test_data, "u", "g", weight = "n", se = T)
    wide <- mutual_local(test_data, "u", "g", weight = "n", wide = T)
    wide_se <- mutual_local(test_data, "u", "g", weight = "n", wide = T, se = T)

    expect_equal(ncol(nowide) + 1, ncol(nowide_se))
    expect_equal(nrow(nowide), 8)
    expect_equal(nrow(nowide), nrow(nowide_se))
    expect_equal(nrow(nowide), nrow(wide) * 2)
    expect_equal(ncol(wide) + 2, ncol(wide_se))
    expect_equal(nrow(wide), nrow(wide_se))

    expect_equal(nowide[nowide$stat=="ls",][["est"]], wide$ls)
    expect_equal(nowide[nowide$stat=="p",][["est"]], wide$p)

    total <- mutual_total(test_data, "u", "g", weight = "n")
    expect_equal(total[["M", "est"]],
                 sum(nowide[nowide$stat=="ls", "est"] * nowide[nowide$stat=="p", "est"]))
    expect_equal(total[["M", "est"]], sum(wide$ls * wide$p))
    expect_equal(total[["H", "est"]], sum(wide$ls * wide$p) / entropy(test_data, "u", "n"))

    expect_equal(all(nowide_se['se'] > 0), TRUE)
})
