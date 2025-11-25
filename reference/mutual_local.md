# Calculates local segregation scores based on M

Returns local segregation indices for each category defined by `unit`.

## Usage

``` r
mutual_local(
  data,
  group,
  unit,
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
  Defines the dimension over which segregation is computed.

- unit:

  A categorical variable or a vector of variables contained in `data`.
  Defines the group for which local segregation indices are calculated.

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

Returns a data.table with two rows for each category defined by `unit`,
for a total of `2*(number of units)` rows. The column `est` contains two
statistics that are provided for each unit: `ls`, the local segregation
score, and `p`, the proportion of the unit from the total number of
cases. If `se` is set to `TRUE`, an additional column `se` contains the
associated bootstrapped standard errors, an additional column `CI`
contains the estimate confidence interval as a list column, an
additional column `bias` contains the estimated bias, and the column
`est` contains the bias-corrected estimates. If `wide` is set to `TRUE`,
returns instead a wide dataframe, with one row for each `unit`, and the
associated statistics in separate columns.

## References

Henri Theil. 1971. Principles of Econometrics. New York: Wiley.

Ricardo Mora and Javier Ruiz-Castillo. 2011. "Entropy-based Segregation
Indices". Sociological Methodology 41(1): 159–194.

## Examples

``` r
# which schools are most segregated?
(localseg <- mutual_local(schools00, "race", "school",
    weight = "n", wide = TRUE
))
#> Key: <school>
#>       school        ls            p
#>       <fctr>     <num>        <num>
#>    1:   A1_1 0.1826710 0.0004522985
#>    2:   A1_2 0.1825592 0.0004978701
#>    3:   A1_3 0.2756157 0.0006642066
#>    4:   A1_4 0.1368034 0.0005685061
#>    5:   A2_1 0.3585546 0.0004260948
#>   ---                              
#> 2041: C165_1 0.3174930 0.0004568556
#> 2042: C165_2 0.3835477 0.0005297702
#> 2043: C165_3 0.2972550 0.0005650883
#> 2044: C166_1 0.3072281 0.0011586588
#> 2045: C167_1 0.3166498 0.0005354667

sum(localseg$p) # => 1
#> [1] 1

# the sum of the weighted local segregation scores equals
# total segregation
sum(localseg$ls * localseg$p) # => .425
#> [1] 0.425539
mutual_total(schools00, "school", "race", weight = "n") # M => .425
#>      stat        est
#>    <char>      <num>
#> 1:      M 0.42553898
#> 2:      H 0.05642991
```
