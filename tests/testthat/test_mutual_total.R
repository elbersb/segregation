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
    mutual_total(test_data, "u", "g", weight = "n"),
    mutual_total(test_data, "g", "u", weight = "n")
  )

  expect_equal(
    mutual_total(test_data, "u", c("supergroup", "g"), weight = "n"),
    mutual_total(test_data, "u", "g", weight = "n")
  )

  expect_equal(
    mutual_total(test_data, "u", c("supergroup", "g"), weight = "n"),
    mutual_total(test_data, c("supergroup", "g"), "u", weight = "n")
  )
})

test_that("between + within = total", {
  expect_equal(
    mutual_total(test_data, "u", "g", weight = "n")$M,
    mutual_total(test_data, "u", "supergroup", weight = "n")$M +
      mutual_total(test_data, "u", "g", within = "supergroup", weight = "n")$M
  )
})

p_12 <- sum(test_data[test_data$supergroup == 12, "n"]) / sum(test_data$n)
p_34 <- sum(test_data[test_data$supergroup == 34, "n"]) / sum(test_data$n)
test_that("within estimations is correct", {
  expect_equal(
    p_12 * mutual_total(test_data[test_data$supergroup == 12, ], "u", "g", weight = "n")$M +
      p_34 * mutual_total(test_data[test_data$supergroup == 34, ], "u", "g", weight = "n")$M,
    mutual_total(test_data, "u", "g", within = "supergroup", weight = "n")$M
  )
})

test_that("bootstrapping works", {
  expect_length(
    mutual_total(test_data, "u", "g", weight = "n", se = TRUE, n_bootstrap = 20),
    3
  )
})
