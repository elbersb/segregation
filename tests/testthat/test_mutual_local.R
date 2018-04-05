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

test_that("local calculation works", {
  expect_equal(sum(local[, "p"]), 1)
  expect_equal(sum(local2[, "p"]), 1)
  expect_equal(sum(localse[, "p"]), 1)

  expect_equal(sum(local[, "M_group"]), sum(local2[, "M_group"]))
  expect_equal(
    sum(local[, "M_group"]),
    mutual_total(test_data, "u", "g", weight = "n")$M
  )
})

test_that("return works", {
  expect_equal(nrow(local), 4)
  expect_equal(ncol(local), 4)
})

test_that("bootstrapping works", {
  expect_equal(nrow(localse), 4)
  expect_equal(ncol(localse), 6)
})
