# Calculates the entropy of a distribution

Returns the entropy of the distribution defined by `group`.

## Usage

``` r
entropy(data, group, within = NULL, weight = NULL, base = exp(1))
```

## Arguments

- data:

  A data frame.

- group:

  A categorical variable or a vector of variables contained in `data`.

- within:

  A categorical variable or a vector of variables contained in `data`.

- weight:

  Numeric. (Default `NULL`)

- base:

  Base of the logarithm that is used in the entropy calculation.
  Defaults to the natural logarithm.

## Value

A single number, the entropy.

## Examples

``` r
d <- data.frame(cat = c("A", "B"), n = c(25, 75))
entropy(d, "cat", weight = "n") # => .56
#> [1] 0.5623351
# this is equivalent to -.25*log(.25)-.75*log(.75)

d <- data.frame(cat = c("A", "B"), n = c(50, 50))
# use base 2 for the logarithm, then entropy is maximized at 1
entropy(d, "cat", weight = "n", base = 2) # => 1
#> [1] 1
```
