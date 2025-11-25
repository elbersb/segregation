# Scree plot for segregation compression

A plot that allows to visually see the effect of compression on mutual
information.

## Usage

``` r
scree_plot(compression, tail = Inf)
```

## Arguments

- compression:

  A "segcompression" object returned by
  [compress](https://elbersb.com/segregation/reference/compress.md).

- tail:

  Return only the last `tail` units (default: `Inf`)

## Value

Returns a ggplot2 plot.
