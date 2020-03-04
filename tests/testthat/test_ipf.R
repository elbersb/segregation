library("segregation")
context("test_ipf")

test_that("different precisions", {
    # reduce to overlap sample
    schools00_r <- schools00[schools00$school %in% schools05$school, ]
    schools05_r <- schools05[schools05$school %in% schools00$school, ]

    for (precision in c(.1, .01, .001)) {
        adj <- ipf(schools00_r, schools05_r, "race", "school", weight = "n", precision = precision)

        # check that the new "race" marginals are similar to the target marginals
        new <- aggregate(adj$n, list(adj$race), sum)[, "x"]
        old <- aggregate(schools05_r$n, list(schools05_r$race), sum)[, "x"]
        new <- new / sum(new)
        old <- old / sum(old)
        expect_true(all(abs(new - old) < precision))

        # check that the new "school" marginals are similar to the target marginals
        new <- aggregate(adj$n, list(adj$school), sum)[, "x"]
        old <- aggregate(schools05_r$n, list(schools05_r$school), sum)[, "x"]
        new <- new / sum(new)
        old <- old / sum(old)
        expect_true(all(abs(new - old) < precision))
    }
})

test_that("warn if iterations are too low", {
    expect_error(
        suppressWarnings(
            ipf(schools00, schools05, "race", "school", weight = "n",
            precision = .00001, max_iterations = 1)))
})

test_that("gives sames results as mutual_difference", {
    diff <- mutual_difference(schools00, schools05, group = "race", unit = "school",
        weight = "n", method = "km", precision = 0.000001)
    # what changed from 2000 to 2005?
    # first reduce to overlap sample
    schools00_r <- schools00[schools00$school %in% schools05$school, ]
    schools05_r <- schools05[schools05$school %in% schools00$school, ]
    M_00 <- mutual_total(schools00_r, "race", "school", weight = "n")[stat == "M", est]
    M_05 <- mutual_total(schools05_r, "race", "school", weight = "n")[stat == "M", est]
    # adjust the 2000 margins to the 2005 margins
    # if only the margins changed, then this would explain all the difference
    adj_00 <- ipf(schools00_r, schools05_r, "race", "school", weight = "n", precision = 0.00001)
    M_margins <- mutual_total(adj_00, "race", "school", weight = "n")[stat == "M", est]
    structural_change <- M_05 - M_margins
    # test
    expect_equal(diff[stat == "M1", est] + diff[stat == "removals", est], M_00)
    expect_equal(diff[stat == "M2", est] - diff[stat == "additions", est], M_05)
    expect_equal(structural_change, diff[stat == "structural", est], tolerance = .001)
    expect_equal(
        M_05 - M_00 - structural_change,
        diff[stat %in% c("unit_marginal", "group_marginal", "interaction"), sum(est)],
        tolerance = .001)
})

test_that("example from Karmel & Maclachlan 1988", {
    source <- data.frame(
        occ = rep(c(1, 2, 3), 2),
        gender = c(rep("male", 3), rep("female", 3)),
        n = c(100, 50, 100, 50, 50, 50)
    )
    target <- data.frame(
        occ = rep(c(1, 2, 3), 2),
        gender = c(rep("male", 3), rep("female", 3)),
        n = c(125, 100, 100, 100, 100, 75)
    )
    adj <- ipf(source, target, "occ", "gender", "n", precision = 0.0000000001)
    # K-M report on the scale of the target distribution
    adj$n <- round(adj$n / sum(adj$n) * sum(adj$n_target), 1)
    expect_equal(adj[adj$gender == "male" & adj$occ == 1, "n"][[1]], 134.7)
    expect_equal(adj[adj$gender == "male" & adj$occ == 2, "n"][[1]], 85.5)
    expect_equal(adj[adj$gender == "male" & adj$occ == 3, "n"][[1]], 104.8)
    expect_equal(adj[adj$gender == "female" & adj$occ == 1, "n"][[1]], 90.3)
    expect_equal(adj[adj$gender == "female" & adj$occ == 2, "n"][[1]], 114.5)
    expect_equal(adj[adj$gender == "female" & adj$occ == 3, "n"][[1]], 70.2)

    expect_equal(sum(adj[adj$gender == "male", "n"]),
                 sum(adj[adj$gender == "male", "n_target"]))
    expect_equal(sum(adj[adj$gender == "female", "n"]),
                 sum(adj[adj$gender == "female", "n_target"]))
    expect_equal(sum(adj[adj$occ == 1, "n"]), sum(adj[adj$occ == 1, "n_target"]))
    expect_equal(sum(adj[adj$occ == 2, "n"]), sum(adj[adj$occ == 2, "n_target"]))
    expect_equal(sum(adj[adj$occ == 3, "n"]), sum(adj[adj$occ == 3, "n_target"]))
})

test_that("warning about units and groups being dropped", {
    expect_warning(ipfd <- ipf(schools00, schools05, "race", "school", weight = "n"))
    expect_equal(sum(ipfd$n), sum(ipfd$n_source))
})

test_that("returns same number of observations as before", {
    # schools are dropped here
    suppressWarnings(ipfd <- ipf(schools00, schools05, "race", "school", weight = "n"))
    expect_equal(sum(ipfd$n), sum(ipfd$n_source))

    # reduce to overlap sample, because then by definition the counts are identical
    schools00_r <- schools00[schools00$school %in% schools05$school, ]
    schools05_r <- schools05[schools05$school %in% schools00$school, ]

    ipfd <- ipf(schools00_r, schools05_r, "race", "school", weight = "n")
    expect_equal(sum(ipfd$n), sum(ipfd$n_source))
    expect_equal(sum(schools00_r$n), sum(ipfd$n))
    expect_equal(sum(schools00_r$n), sum(ipfd$n_source))
    expect_equal(sum(schools05_r$n), sum(ipfd$n_target))
})
