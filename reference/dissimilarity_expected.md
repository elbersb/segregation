# Calculates expected values when true segregation is zero

When sample sizes are small, one group has a small proportion, or when
there are many units, segregation indices are typically upwardly biased,
even when true segregation is zero. This function simulates tables with
zero segregation, given the marginals of the dataset, and calculates
segregation. If the expected values are large, the interpretation of
index scores might have to be adjusted.

## Usage

``` r
dissimilarity_expected(
  data,
  group,
  unit,
  weight = NULL,
  fixed_margins = TRUE,
  n_bootstrap = 100
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
  Defines the second dimension over which segregation is computed.

- weight:

  Numeric. (Default `NULL`)

- fixed_margins:

  Should the margins be fixed or simulated? (Default `TRUE`)

- n_bootstrap:

  Number of bootstrap iterations. (Default `100`)

## Value

A data.table with one row, corresponding to the expected value of the D
index when true segregation is zero.

## Examples

``` r
# build a smaller table, with 100 students distributed across
# 10 schools, where one racial group has 10% of the students
small <- data.frame(
    school = c(1:10, 1:10),
    race = c(rep("r1", 10), rep("r2", 10)),
    n = c(rep(1, 10), rep(9, 10))
)
dissimilarity_expected(small, "race", "school", weight = "n")
#>         stat       est        se
#>       <char>     <num>     <num>
#> 1: D under 0 0.3855556 0.1052968
# with an increase in sample size (n=1000), the values improve
small$n <- small$n * 10
dissimilarity_expected(small, "race", "school", weight = "n")
#>         stat       est         se
#>       <char>     <num>      <num>
#> 1: D under 0 0.1232222 0.03038166
```
