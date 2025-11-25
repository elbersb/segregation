# Calculates pairwise exposure indices

Returns the pairwise exposure indices between groups

## Usage

``` r
exposure(data, group, unit, weight = NULL)
```

## Arguments

- data:

  A data frame.

- group:

  A categorical variable contained in `data`. Defines the first
  dimension over which segregation is computed.

- unit:

  A vector of variables contained in `data`. Defines the second
  dimension over which segregation is computed.

- weight:

  Numeric. (Default `NULL`)

## Value

Returns a data.table with columns "of", "to", and "exposure". Read
results as "exposure of group x to group y".
