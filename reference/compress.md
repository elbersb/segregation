# Compresses a data matrix based on mutual information (segregation)

Given a data set that identifies suitable neighbors for merging, this
function will merge units iteratively, where in each iteration the
neighbors with the smallest reduction in terms of total M will be
merged.

## Usage

``` r
compress(
  data,
  group,
  unit,
  weight = NULL,
  neighbors = "local",
  n_neighbors = 50,
  max_iter = Inf
)
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

  Numeric. Only frequency weights are allowed. (Default `NULL`)

- neighbors:

  Either a data frame or a character. If data frame, then it needs
  exactly two columns, where each row identifies a set of "neighbors"
  that may be merged. If "local", considers the `n_neighbors` closest
  neighbors in terms of local segregation. If "all", all units are
  considered as possible neighbors. This may be very time-consuming.

- n_neighbors:

  Only relevant if `neighbors` is `"local"`.

- max_iter:

  Maximum number of iterations (Default `Inf`)

## Value

Returns a data.table.
