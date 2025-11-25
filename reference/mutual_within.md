# Calculates detailed within-category segregation scores for M and H

Calculates the segregation between `group` and `unit` within each
category defined by `within`.

## Usage

``` r
mutual_within(
  data,
  group,
  unit,
  within,
  weight = NULL,
  se = FALSE,
  CI = 0.95,
  n_bootstrap = 100,
  base = exp(1),
  wide = FALSE
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

- within:

  A categorical variable or a vector of variables contained in `data`
  that defines the within-segregation categories.

- weight:

  Numeric. (Default `NULL`)

- se:

  If `TRUE`, the segregation estimates are bootstrapped to provide
  standard errors and to apply bias correction. The bias that is
  reported has already been applied to the estimates (i.e. the reported
  estimates are "debiased") (Default `FALSE`)

- CI:

  If `se = TRUE`, compute the confidence (CI\*100) in addition to the
  bootstrap standard error. This is based on percentiles of the
  bootstrap distribution, and a valid interpretation relies on a larger
  number of bootstrap iterations. (Default `0.95`)

- n_bootstrap:

  Number of bootstrap iterations. (Default `100`)

- base:

  Base of the logarithm that is used in the calculation. Defaults to the
  natural logarithm.

- wide:

  Returns a wide dataframe instead of a long dataframe. (Default
  `FALSE`)

## Value

Returns a data.table with four rows for each category defined by
`within`. The column `est` contains four statistics that are provided
for each unit: `M` is the within-category M, and `p` is the proportion
of the category. Multiplying `M` and `p` gives the contribution of each
within-category towards the total M. `H` is the within-category H, and
`ent_ratio` provides the entropy ratio, defined as `EW/E`, where `EW` is
the within-category entropy, and `E` is the overall entropy. Multiplying
`H`, `p`, and `ent_ratio` gives the contribution of each within-category
towards the total H. If `se` is set to `TRUE`, an additional column `se`
contains the associated bootstrapped standard errors, an additional
column `CI` contains the estimate confidence interval as a list column,
an additional column `bias` contains the estimated bias, and the column
`est` contains the bias-corrected estimates. If `wide` is set to `TRUE`,
returns instead a wide dataframe, with one row for each `within`
category, and the associated statistics in separate columns.

## References

Henri Theil. 1971. Principles of Econometrics. New York: Wiley.

Ricardo Mora and Javier Ruiz-Castillo. 2011. "Entropy-based Segregation
Indices". Sociological Methodology 41(1): 159–194.

## Examples

``` r
if (FALSE) { # \dontrun{
(within <- mutual_within(schools00, "race", "school",
    within = "state",
    weight = "n", wide = TRUE
))
# the M for state "A" is .409
# manual calculation
schools_A <- schools00[schools00$state == "A", ]
mutual_total(schools_A, "race", "school", weight = "n") # M => .409

# to recover the within M and H from the output, multiply
# p * M and p * ent_ratio * H, respectively
sum(within$p * within$M) # => .326
sum(within$p * within$ent_ratio * within$H) # => .321
# compare with:
mutual_total(schools00, "race", "school", within = "state", weight = "n")
} # }
```
