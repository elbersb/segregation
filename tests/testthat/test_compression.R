library("segregation")
context("test_compression")

neighbors <- unique(schools00$school)[1:10]
neighbors <- expand.grid(a = neighbors, b = neighbors)
res <- compress(schools00, neighbors, "race", "school", weight = "n")

test_that("compress works", {
    # 9 merges
    expect_equal(nrow(res), 9)
    # M values is declining continously
    expect_equal(all(res$M[2:9] < res$M[1:8]), TRUE)
    # number of units are correct
    expect_equal(res$N_units[[1]], length(unique(schools00$school)) - 1)
})

test_that("compress_crosswalk works", {
    expect_error(compress_crosswalk(schools00))
    expect_error(compress_crosswalk(res, -1))
    expect_error(compress_crosswalk(res, 10))
    expect_equal(nrow(compress_crosswalk(res, 1)), 2)
    expect_equal(nrow(compress_crosswalk(res, 2)), 4)
    expect_equal(compress_crosswalk(res, 9), compress_crosswalk(res))
})
