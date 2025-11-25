# Turns a contingency table into long format

Returns a data.table in long form, such that it is suitable for use in
[mutual_total](https://elbersb.com/segregation/reference/mutual_total.md),
etc. Colnames and rownames of the matrix will be respected.

## Usage

``` r
matrix_to_long(
  matrix,
  group = "group",
  unit = "unit",
  weight = "n",
  drop_zero = TRUE
)
```

## Arguments

- matrix:

  A matrix, where the rows represent the units, and the column represent
  the groups.

- group:

  Variable name for group. (Default `group`)

- unit:

  Variable name for unit. (Default `unit`)

- weight:

  Variable name for frequency weight. (Default `weight`)

- drop_zero:

  Drop unit-group combinations with zero weight. (Default `TRUE`)

## Value

A data.table.

## Examples

``` r
m <- matrix(c(10, 20, 30, 30, 20, 10), nrow = 3)
colnames(m) <- c("Black", "White")
long <- matrix_to_long(m, group = "race", unit = "school")
mutual_total(long, "race", "school", weight = "n")
#>      stat        est
#>    <char>      <num>
#> 1:      M 0.08720802
#> 2:      H 0.12581458
```
