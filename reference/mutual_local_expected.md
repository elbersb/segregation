# Calculates expected local segregation scores when true segregation is zero

When sample sizes are small, one group has a small proportion, or when
there are many units, segregation indices are typically upwardly biased,
even when true segregation is zero. This function simulates tables with
zero segregation, given the marginals of the dataset, and calculates
local segregation scores. If the expected values are large, the
interpretation of index scores might have to be adjusted.

## Usage

``` r
mutual_local_expected(
  data,
  group,
  unit,
  weight = NULL,
  fixed_margins = TRUE,
  n_bootstrap = 100,
  base = exp(1)
)
```

## Arguments

- data:

  A data frame.

- group:

  A categorical variable or a vector of variables contained in `data`.
  Defines the first dimension over which segregation is computed.

- unit:

  A categorical variable or a vector of variables contained in `data`.
  Defines the group for which local segregation indices are calculated.

- weight:

  Numeric. (Default `NULL`)

- fixed_margins:

  Should the margins be fixed or simulated? (Default `TRUE`)

- n_bootstrap:

  Number of bootstrap iterations. (Default `100`)

- base:

  Base of the logarithm that is used in the calculation. Defaults to the
  natural logarithm.

## Value

A data.table with two rows, corresponding to the expected values of
segregation when true segregation is zero.

## Examples

``` r
if (FALSE) { # \dontrun{
# the schools00 dataset has a large sample size, so expected segregation is close to zero
mutual_local_expected(schools00, "race", "school", weight = "n")

# but we can build a smaller table, with 100 students distributed across
# 10 schools, where one racial group has 10% of the students
small <- data.frame(
    school = c(1:10, 1:10),
    race = c(rep("r1", 10), rep("r2", 10)),
    n = c(rep(1, 10), rep(9, 10))
)
mutual_local_expected(small, "race", "school", weight = "n")
# with an increase in sample size (n=1000), the values improve
small$n <- small$n * 10
mutual_local_expected(small, "race", "school", weight = "n")
} # }
```
