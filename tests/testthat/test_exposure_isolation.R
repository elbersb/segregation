if (!identical(Sys.getenv("NOT_CRAN"), "true")) {
    return()
}

library("segregation")
context("test_exposure_isolation")

test_that("two group case", {
    two <- data.table::as.data.table(schools00)
    two <- two[race %in% c("white", "black")]
    exp <- exposure(two, "race", "school", "n")
    # relationship: sum of xPy and yPx = 1
    expect_equal(exp[, .(sum = sum(exposure)), by = .(of)][["sum"]], c(1, 1))
    # relationship: xPy = yPx Y/X
    white_total <- two[race == "white", sum(n)]
    black_total <- two[race == "black", sum(n)]
    expect_equal(
        exp[of == "black" & to == "white", exposure],
        exp[of == "white" & to == "black", exposure] * white_total / black_total
    )
})

test_that("exposure", {
    exp <- exposure(schools00, "race", "school", "n")
    expect_equal(
        exp[, .(sum = sum(exposure)), by = .(of)][["sum"]],
        rep(1, 5)
    )
})

test_that("exposure and isolation", {
    exp <- exposure(schools00, "race", "school", "n")[of == to]
    iso <- isolation(schools00, "race", "school", "n")
    comp <- merge(exp, iso, by.x = "of", by.y = "race")
    expect_equal(comp[["isolation"]], comp[["exposure"]])
})
