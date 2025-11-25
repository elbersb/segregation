# A visual representation of two-group segregation

Produces one or several segregation curves, as defined in Duncan and
Duncan (1955)

## Usage

``` r
segcurve(data, group, unit, weight = NULL, segment = NULL)
```

## Arguments

- data:

  A data frame.

- group:

  A categorical variable contained in `data`. Defines the first
  dimension over which segregation is computed.

- unit:

  A categorical variable contained in `data`. Defines the second
  dimension over which segregation is computed.

- weight:

  Numeric. (Default `NULL`)

- segment:

  A categorical variable contained in `data`. (Default `NULL`) If given,
  several segregation curves will be shown, one for each segment.

## Value

Returns a ggplot2 object.
