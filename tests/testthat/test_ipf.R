library(segregation)
context("test_ipf")

test_that("different precisions", {
    for(precision in c(.1, .01, .001)) {
        adj <- ipf(schools00, schools05, "race", "school", weight = "n", precision = precision)

        # check that the new "race" marginals are similar to the target marginals
        new <- aggregate(adj$n, list(adj$race), sum)[, "x"]
        old <- aggregate(adj$n_target, list(adj$race), sum)[, "x"]
        
        for(i in 1:length(new)) {
            sorted <- sort(c(old[i], new[i]), decreasing = TRUE)
            ratio <- sorted[1]/sorted[2]
            expect_equal(ratio < (1 + precision), TRUE)
        }
    }
})

test_that("warn if iterations are too low", {
    expect_warning(ipf(schools00, schools05, "race", "school", weight = "n", 
        precision = .00001, max_iterations = 1))
})

test_that("gives sames results as mutual_difference", {
    diff <- mutual_difference(schools00, schools05, group = "race", unit = "school",
        weight = "n", method = "ipf", precision = .1, forward_only = TRUE)
    # what changed from 2000 to 2005?
    # first reduce to overlap sample
    schools00_r = schools00[schools00$school %in% schools05$school,]
    schools05_r = schools05[schools05$school %in% schools00$school,]
    M_00 <- mutual_total(schools00_r, "race", "school", weight = "n")[["M", "est"]]
    M_05 <- mutual_total(schools05_r, "race", "school", weight = "n")[["M", "est"]]
    # adjust the 2000 margins to the 2005 margins
    # if only the margins changed, then this would explain all the difference
    adj_00 <- ipf(schools00_r, schools05_r, "race", "school", weight = "n", precision = .1)
    M_margins <- mutual_total(adj_00, "race", "school", weight = "n")[["M", "est"]]
    structural_change <- M_05 - M_margins
    # test
    expect_equal(diff[["M1", "est"]] + diff[["removals", "est"]], M_00)
    expect_equal(diff[["M2", "est"]] - diff[["additions", "est"]], M_05)
    expect_equal(structural_change, diff[["structural", "est"]])
    expect_equal(M_05 - M_00 - structural_change, 
        sum(diff[c("unit_marginal", "group_marginal", "interaction"), "est"]))
})
