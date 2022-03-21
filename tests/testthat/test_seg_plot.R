library("segregation")
context("seg_plot")

skip_if(!requireNamespace("ggplot2", quietly = TRUE))

plot_majority <- seg_plot(schools00, "race", "school", weight = "n", order = "majority")
plot_seg <- seg_plot(schools00, "race", "school", weight = "n", order = "segregation")
plot_entropy <- seg_plot(schools00, "race", "school",
    weight = "n", order = "entropy",
    bar_space = 0.1
)

test_that("dimensions", {
    expect_equal(nrow(plot_majority$data), nrow(plot_seg$data))
    expect_equal(nrow(plot_majority$data), nrow(plot_entropy$data))
})

test_that("reference", {
    reference <- data.table::as.data.table(schools00)
    reference <- reference[, .(N = sum(n)), by = .(race)]
    reference[, p := N / sum(N)]
    expect_error(seg_plot(schools00, "race", "school",
        weight = "n", order = "segregation",
        reference_distribution = reference
    ))
    expect_error(seg_plot(schools00, "race", "school",
        weight = "n", order = "segregation",
        reference_distribution = reference[1:2, ]
    ))
    expect_error(seg_plot(schools00, "race", "school",
        weight = "n", order = "segregation",
        reference_distribution = 1
    ))

    reference[, N := NULL]
    sp <- seg_plot(schools00, "race", "school",
        weight = "n", order = "segregation"
    )
    sp_ref <- seg_plot(schools00, "race", "school",
        weight = "n", order = "segregation",
        reference_distribution = reference
    )
    # identical order
    expect_equal(sp$data[["unit"]], sp_ref$data[["unit"]])
})