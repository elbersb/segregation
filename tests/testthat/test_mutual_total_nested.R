if (!identical(Sys.getenv("NOT_CRAN"), "true")) {
    return()
}

library("segregation")
context("test_mutual_total_nested")

test_that("works both ways around", {
    decomp <- mutual_total_nested(schools00, "race",
        c("state", "district", "school"),
        weight = "n"
    )
    term1 <- mutual_total(schools00, "race", "state", weight = "n")$est
    term2 <- mutual_total(schools00, "race", "district", within = "state", weight = "n")$est
    term3 <- mutual_total(schools00, "race", "school", within = c("state", "district"), weight = "n")$est
    expect_equal(decomp$est, c(term1, term2, term3))
})
