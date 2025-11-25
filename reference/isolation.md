# Calculates isolation indices

Returns isolation index of each group

## Usage

``` r
isolation(data, group, unit, weight = NULL)
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

Returns a data.table with group column and isolation index.
