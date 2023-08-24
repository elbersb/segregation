if (!identical(Sys.getenv("NOT_CRAN"), "true")) {
    return()
}

library("segregation")
context("plots")

skip_if(!requireNamespace("ggplot2", quietly = TRUE))

plot_majority <- segplot(schools00, "race", "school", weight = "n", order = "majority")
plot_majority_fixed <- segplot(schools00, "race", "school", weight = "n", order = "majority_fixed")
plot_seg <- segplot(schools00, "race", "school", weight = "n", order = "segregation")
plot_entropy <- segplot(schools00, "race", "school",
    weight = "n", order = "entropy",
    bar_space = 0.1
)

test_that("dimensions", {
    expect_equal(nrow(plot_majority$data), nrow(plot_seg$data))
    expect_equal(nrow(plot_majority$data), nrow(plot_entropy$data))
    expect_equal(nrow(plot_majority$data), nrow(plot_majority_fixed$data))
})

test_that("reference", {
    reference <- data.table::as.data.table(schools00)
    reference <- reference[, .(N = sum(n)), by = .(race)]
    reference[, p := N / sum(N)]
    expect_error(segplot(schools00, "race", "school",
        weight = "n", order = "segregation",
        reference_distribution = reference
    ))
    expect_error(segplot(schools00, "race", "school",
        weight = "n", order = "segregation",
        reference_distribution = reference[1:2, ]
    ))
    expect_error(segplot(schools00, "race", "school",
        weight = "n", order = "segregation",
        reference_distribution = 1
    ))

    reference[, N := NULL]
    sp <- segplot(schools00, "race", "school",
        weight = "n", order = "segregation"
    )
    sp_ref <- segplot(schools00, "race", "school",
        weight = "n", order = "segregation",
        reference_distribution = reference
    )
    # identical order
    expect_equal(sp$data[["unit"]], sp_ref$data[["unit"]])
})

test_that("axis_labels", {
    left <- segplot(schools00, "race", "school", weight = "n", axis_labels = "left")
    right <- segplot(schools00, "race", "school", weight = "n", axis_labels = "right")
    both <- segplot(schools00, "race", "school", weight = "n", axis_labels = "both")
    expect_equal(nrow(left$data), nrow(right$data))
    expect_equal(nrow(both$data), nrow(right$data))
})

test_that("segcurve", {
    expect_error(segcurve(schools00, "race", "school", weight = "n"))

    p1 <- segcurve(subset(schools00, race %in% c("white", "black")),
        "race", "school",
        weight = "n"
    )
    p2 <- segcurve(subset(schools00, race %in% c("white", "asian")),
        "race", "school",
        weight = "n"
    )
})
