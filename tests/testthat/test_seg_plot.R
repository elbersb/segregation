library("segregation")
context("seg_plot")

skip_if(!requireNamespace("ggplot2", quietly = TRUE))

plot_majority <- seg_plot(schools00, "race", "school", weight = "n", order = "majority")
plot_seg <- seg_plot(schools00, "race", "school", weight = "n", order = "segregation")

test_that("dimensions", {
    expect_equal(nrow(plot_majority$data), nrow(plot_seg$data))
})
