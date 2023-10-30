if (!identical(Sys.getenv("NOT_CRAN"), "true")) {
    return()
}

library("segregation")
context("plots")

skip_if(!requireNamespace("ggplot2", quietly = TRUE))
skip_if(!requireNamespace("patchwork", quietly = TRUE))

plot_majority <- segplot(schools00, "race", "school", weight = "n", order = "majority", hline = "white")
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

test_that("secondary plot", {
    plot_2a <- segplot(schools00, "race", "school",
        weight = "n",
        order = "segregation", secondary_plot = "segregation"
    )
    plot_2b <- segplot(schools00, "race", "school",
        weight = "n",
        order = "segregation", secondary_plot = "cumulative"
    )

    expect_equal(plot_seg$data, plot_2a[[1]]$data)
    expect_equal(plot_seg$data, plot_2b[[1]]$data)
    expect_true("patchwork" %in% class(plot_2a))
    expect_true("patchwork" %in% class(plot_2b))
})

test_that("segcurve", {
    expect_error(segcurve(schools00, "race", "school", weight = "n"))
    expect_error(segcurve(schools00, "race", "school", weight = "n", segment = c("a", "b")))
    expect_error(segcurve(schools00, "race", "school", weight = "n", segment = c("state", "school")))

    p1 <- segcurve(subset(schools00, race %in% c("white", "black")),
        "race", "school",
        weight = "n"
    )
    p2 <- segcurve(subset(schools00, race %in% c("white", "asian")),
        "race", "school",
        weight = "n"
    )
    p3 <- segcurve(subset(schools00, race %in% c("white", "asian")),
        "race", "school",
        weight = "n",
        segment = "state"
    )
    expect_equal(p3$labels$colour, "state")
})
