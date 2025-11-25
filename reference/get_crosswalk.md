# Create crosswalk after compression

After running
[compress](https://elbersb.com/segregation/reference/compress.md), this
function creates a crosswalk table. Usually it is preferred to call
[merge_units](https://elbersb.com/segregation/reference/merge_units.md)
directly.

## Usage

``` r
get_crosswalk(compression, n_units = NULL, percent = NULL, parts = FALSE)
```

## Arguments

- compression:

  A "segcompression" object returned by
  [compress](https://elbersb.com/segregation/reference/compress.md).

- n_units:

  Determines the number of merges by specifying the number of units to
  remain in the compressed dataset. Only `n_units` or `percent` must be
  given. (default: `NULL`)

- percent:

  Determines the number of merges by specifying the percentage of total
  segregation information retained in the compressed dataset. Only
  `n_units` or `percent` must be given. (default: `NULL`)

- parts:

  (default: FALSE)

## Value

Returns a ggplot2 plot.

Returns a data.table.
