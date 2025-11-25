# Calculates the Mutual Information Index M and Theil's Entropy Index H

Returns the total segregation between `group` and `unit`. If `within` is
given, calculates segregation within each `within` category separately,
and takes the weighted average. Also see
[`mutual_within`](https://elbersb.com/segregation/reference/mutual_within.md)
for detailed within calculations.

## Usage

``` r
mutual_total(
  data,
  group,
  unit,
  within = NULL,
  weight = NULL,
  se = FALSE,
  CI = 0.95,
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
  Defines the second dimension over which segregation is computed.

- within:

  A categorical variable or a vector of variables contained in `data`.
  The variable(s) should be a superset of either the `unit` or the
  `group` for the calculation to be meaningful. If provided, segregation
  is computed within the groups defined by the variable, and then
  averaged. (Default `NULL`)

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

## Value

Returns a data.table with two rows. The column `est` contains the Mutual
Information Index, M, and Theil's Entropy Index, H. The H is the the M
divided by the `group` entropy. If `within` was given, M and H are
weighted averages of the within-category segregation scores. If `se` is
set to `TRUE`, an additional column `se` contains the associated
bootstrapped standard errors, an additional column `CI` contains the
estimate confidence interval as a list column, an additional column
`bias` contains the estimated bias, and the column `est` contains the
bias-corrected estimates.

## References

Henri Theil. 1971. Principles of Econometrics. New York: Wiley.

Ricardo Mora and Javier Ruiz-Castillo. 2011. "Entropy-based Segregation
Indices". Sociological Methodology 41(1): 159–194.

## Examples

``` r
# calculate school racial segregation
mutual_total(schools00, "school", "race", weight = "n") # M => .425
#>      stat        est
#>    <char>      <num>
#> 1:      M 0.42553898
#> 2:      H 0.05642991

# note that the definition of groups and units is arbitrary
mutual_total(schools00, "race", "school", weight = "n") # M => .425
#>      stat       est
#>    <char>     <num>
#> 1:      M 0.4255390
#> 2:      H 0.4188083

# if groups or units are defined by a combination of variables,
# vectors of variable names can be provided -
# here there is no difference, because schools
# are nested within districts
mutual_total(schools00, "race", c("district", "school"),
    weight = "n"
) # M => .424
#>      stat       est
#>    <char>     <num>
#> 1:      M 0.4255390
#> 2:      H 0.4188083

# estimate standard errors and 95% CI for M and H
if (FALSE) { # \dontrun{
mutual_total(schools00, "race", "school",
    weight = "n",
    se = TRUE, n_bootstrap = 1000
)

# estimate segregation within school districts
mutual_total(schools00, "race", "school",
    within = "district", weight = "n"
) # M => .087

# estimate between-district racial segregation
mutual_total(schools00, "race", "district", weight = "n") # M => .338
# note that the sum of within-district and between-district
# segregation equals total school-race segregation;
# here, most segregation is between school districts
} # }
```
