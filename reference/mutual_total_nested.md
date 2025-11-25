# Calculates a nested decomposition of segregation for M and H

Returns the between-within decomposition defined by the sequence of
variables in `unit`.

## Usage

``` r
mutual_total_nested(data, group, unit, weight = NULL, base = exp(1))
```

## Arguments

- data:

  A data frame.

- group:

  A categorical variable or a vector of variables contained in `data`.
  Defines the first dimension over which segregation is computed.

- unit:

  A vector of variables contained in `data`. Defines the levels at which
  the decomposition should be computed.

- weight:

  Numeric. (Default `NULL`)

- base:

  Base of the logarithm that is used in the calculation. Defaults to the
  natural logarithm.

## Value

Returns a data.table similar to
[`mutual_total`](https://elbersb.com/segregation/reference/mutual_total.md),
but with column `between` and `within` that define the levels of
nesting.

## Examples

``` r
mutual_total_nested(schools00, "race", c("state", "district", "school"),
    weight = "n"
)
#>     between          within   stat        est
#>      <char>          <char> <char>      <num>
#> 1:    state                      M 0.09924370
#> 2:    state                      H 0.09767398
#> 3: district           state      M 0.23870880
#> 4: district           state      H 0.23493319
#> 5:   school state, district      M 0.08758648
#> 6:   school state, district      H 0.08620114
# This is a simpler way to run the following manually:
# mutual_total(schools00, "race", "state", weight = "n")
# mutual_total(schools00, "race", "district", within = "state", weight = "n")
# mutual_total(schools00, "race", "school", within = c("state", "district"), weight = "n")
```
