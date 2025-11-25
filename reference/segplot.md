# A visual representation of segregation

Produces a segregation plot.

## Usage

``` r
segplot(
  data,
  group,
  unit,
  weight,
  order = "segregation",
  secondary_plot = NULL,
  reference_distribution = NULL,
  bar_space = 0,
  hline = NULL
)
```

## Arguments

- data:

  A data frame.

- group:

  A categorical variable or a vector of variables contained in `data`.
  Defines the first dimension over which segregation is computed.

- unit:

  A categorical variable or a vector of variables contained in `data`.
  Defines the second dimension over which segregation is computed.

- weight:

  Numeric. (Default `NULL`)

- order:

  A character, either "segregation", "entropy", "majority", or
  "majority_fixed". Affects the ordering of the units. The horizontal
  ordering of the groups can be changed by using a factor variable for
  `group`. The difference between "majority" and "majority_fixed" is
  that the former will reorder the groups in such a way that the
  majority group actually comes first. If you want to control the
  ordering yourself, use "majority_fixed" and specify the `group`
  variable as a factor variable.

- secondary_plot:

  If `NULL` (default), no secondary plot is drawn. If "segregation", a
  secondary plot is drawn that shows adjusted local segregation scores
  for each unit. If "cumulative", a secondary plot is drawn that shows
  the cumulative contribution of each unit toward the total H
  (calculated as the proportion of each unit times the adjusted local
  segregation of each unit)0.

- reference_distribution:

  Specifies the reference distribution, given as a two-column data
  frame, to be plotted on the right. If order is `segregation`, then
  this reference distribution is also used to compute the local
  segregation scores.

- bar_space:

  Specifies space between single units.

- hline:

  Default `NULL`. If a color is specified, horizontal lines will be
  drawn where groups are separated.

## Value

Returns a ggplot2 or patchwork object.
