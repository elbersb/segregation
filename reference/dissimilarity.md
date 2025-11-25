# Calculates Index of Dissimilarity

Returns the total segregation between `group` and `unit` using the Index
of Dissimilarity.

## Usage

``` r
dissimilarity(
  data,
  group,
  unit,
  weight = NULL,
  se = FALSE,
  CI = 0.95,
  n_bootstrap = 100
)
```

## Arguments

- data:

  A data frame.

- group:

  A categorical variable or a vector of variables contained in `data`.
  Defines the first dimension over which segregation is computed. The D
  index only allows two distinct groups.

- unit:

  A categorical variable or a vector of variables contained in `data`.
  Defines the second dimension over which segregation is computed.

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

## Value

Returns a data.table with one row. The column `est` contains the Index
of Dissimilarity. If `se` is set to `TRUE`, an additional column `se`
contains the associated bootstrapped standard errors, an additional
column `CI` contains the estimate confidence interval as a list column,
an additional column `bias` contains the estimated bias, and the column
`est` contains the bias-corrected estimates.

## References

Otis Dudley Duncan and Beverly Duncan. 1955. "A Methodological Analysis
of Segregation Indexes," American Sociological Review 20(2): 210-217.

## Examples

``` r
# Example where D and H deviate
m1 <- matrix_to_long(matrix(c(100, 60, 40, 0, 0, 40, 60, 100), ncol = 2))
m2 <- matrix_to_long(matrix(c(80, 80, 20, 20, 20, 20, 80, 80), ncol = 2))
dissimilarity(m1, "group", "unit", weight = "n")
#>      stat   est
#>    <char> <num>
#> 1:      D   0.6
dissimilarity(m2, "group", "unit", weight = "n")
#>      stat   est
#>    <char> <num>
#> 1:      D   0.6
```
