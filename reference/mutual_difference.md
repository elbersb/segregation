# Decomposes the difference between two M indices

Uses one of three methods to decompose the difference between two M
indices: (1) "shapley" / "shapley_detailed": a method based on the
Shapley decomposition with a few advantages over the Karmel-Maclachlan
method (recommended and the default, Deutsch et al. 2006), (2) "km": the
method based on Karmel-Maclachlan (1988), (3) "mrc": the method
developed by Mora and Ruiz-Castillo (2009). All methods have been
extended to account for missing units/groups in either data input.

## Usage

``` r
mutual_difference(
  data1,
  data2,
  group,
  unit,
  weight = NULL,
  method = "shapley",
  se = FALSE,
  CI = 0.95,
  n_bootstrap = 100,
  base = exp(1),
  ...
)
```

## Arguments

- data1:

  A data frame with same structure as `data2`.

- data2:

  A data frame with same structure as `data1`.

- group:

  A categorical variable or a vector of variables contained in `data`.
  Defines the first dimension over which segregation is computed.

- unit:

  A categorical variable or a vector of variables contained in `data`.
  Defines the second dimension over which segregation is computed.

- weight:

  Numeric. (Default `NULL`)

- method:

  Either "shapley" (the default), "km" (Karmel and Maclachlan method),
  or "mrc" (Mora and Ruiz-Castillo method).

- se:

  If `TRUE`, the segregation estimates are bootstrapped to provide
  standard errors and to apply bias correction. The bias that is
  reported has already been applied to the estimates (i.e. the reported
  estimates are "debiased") (Default `FALSE`)

- CI:

  If `se = TRUE`, compute the confidence (CI\*100) in addition to the
  bootstrap standard error. This is based on percentiles of the
  bootstrap distribution, and a valid interpretation relies on a larger
  number of bootstrap iterations. (Default `0.95`)

- n_bootstrap:

  Number of bootstrap iterations. (Default `100`)

- base:

  Base of the logarithm that is used in the calculation. Defaults to the
  natural logarithm.

- ...:

  Only used for additional arguments when when `method` is set to
  `shapley` or `km`. See
  [ipf](https://elbersb.com/segregation/reference/ipf.md) for details.

## Value

Returns a data.table with columns `stat` and `est`. The data frame
contains the following rows defined by `stat`: `M1` contains the M for
`data1`. `M2` contains the M for `data2`. `diff` is the difference
between `M2` and `M1`. The sum of the five rows following `diff` equal
`diff`.

`additions` contains the change in M induces by `unit` and `group`
categories present in `data2` but not `data1`, and `removals` the
reverse.

All methods return the following three terms: `unit_marginal` is the
contribution of unit composition differences. `group_marginal` is the
contribution of group composition differences. `structural` is the
contribution unexplained by the marginal changes, i.e. the structural
difference. Note that the interpretation of these terms depend on the
exact method used.

When using "km", one additional row is returned: `interaction` is the
contribution of differences in the joint marginal distribution of `unit`
and `group`.

When "shapley_detailed" is used, an additional column "unit" is
returned, along with six additional rows for each unit that is present
in both `data1` and `data2`. The five rows have the following meaning:
`p1` (`p2`) is the proportion of the unit in `data1` (`data2`) once
non-intersecting units/groups have been removed. The changes in local
linkage are given by `ls_diff1` and `ls_diff2`, and their average is
given by `ls_diff_mean`. The row named `total` summarizes the
contribution of the unit towards structural change using the formula
`.5 * p1 * ls_diff1 + .5 * p2 * ls_diff2`. The sum of all "total"
components equals structural change.

If `se` is set to `TRUE`, an additional column `se` contains the
associated bootstrapped standard errors, an additional column `CI`
contains the estimate confidence interval as a list column, an
additional column `bias` contains the estimated bias, and the column
`est` contains the bias-corrected estimates.

## Details

The Shapley method is an improvement over the Karmel-Maclachlan method
(Deutsch et al. 2006). It is based on several margins-adjusted data
inputs and yields symmetrical results (i.e. `data1` and `data2` can be
switched). When "shapley_detailed" is used, the structural component is
further decomposed into the contributions of individuals units.

The Karmel-Maclachlan method (Karmel and Maclachlan 1988) adjusts the
margins of `data1` to be similar to the margins of `data2`. This process
is not symmetrical.

The Shapley and Karmel-Maclachlan methods are based on iterative
proportional fitting (IPF), first introduced by Deming and Stephan
(1940). Depending on the size of the dataset, this may take a few
seconds (see [ipf](https://elbersb.com/segregation/reference/ipf.md) for
details).

The method developed by Mora and Ruiz-Castillo (2009) uses an algebraic
approach to estimate the size of the components. This will often yield
substantively different results from the Shapley and Karmel-Maclachlan
methods. Note that this method is not symmetric in terms of what is
defined as `group` and `unit` categories, which may yield contradictory
results.

A problem arises when there are `group` and/or `unit` categories in
`data1` that are not present in `data2` (or vice versa). All methods
estimate the difference only for categories that are present in both
datasets, and report additionally the change in M that is induced by
these cases as `additions` (present in `data2`, but not in `data1`) and
`removals` (present in `data1`, but not in `data2`).

## References

W. E. Deming, F. F. Stephan. 1940. "On a Least Squares Adjustment of a
Sampled Frequency Table When the Expected Marginal Totals are Known."
The Annals of Mathematical Statistics 11(4): 427-444.

T. Karmel and M. Maclachlan. 1988. "Occupational Sex Segregation —
Increasing or Decreasing?" Economic Record 64: 187-195.

R. Mora and J. Ruiz-Castillo. 2009. "The Invariance Properties of the
Mutual Information Index of Multigroup Segregation." Research on
Economic Inequality 17: 33-53.

J. Deutsch, Y. Flückiger, and J. Silber. 2009. "Analyzing Changes in
Occupational Segregation: The Case of Switzerland (1970–2000)." Research
on Economic Inequality 17: 171–202.

## Examples

``` r
if (FALSE) { # \dontrun{
# decompose the difference in school segregation between 2000 and 2005,
# using the Shapley method
mutual_difference(schools00, schools05,
    group = "race", unit = "school",
    weight = "n", method = "shapley", precision = .1
)
# => the structural component is close to zero, thus most change is in the marginals.
# This method gives identical results when we switch the unit and group definitions,
# and when we switch the data inputs.

# the Karmel-Maclachlan method is similar, but only adjust the data in the forward direction...
mutual_difference(schools00, schools05,
    group = "school", unit = "race",
    weight = "n", method = "km", precision = .1
)

# ...this means that the results won't be identical when we switch the data inputs
mutual_difference(schools05, schools00,
    group = "school", unit = "race",
    weight = "n", method = "km", precision = .1
)

# the MRC method indicates a much higher structural change...
mutual_difference(schools00, schools05,
    group = "race", unit = "school",
    weight = "n", method = "mrc"
)

# ...and is not symmetric
mutual_difference(schools00, schools05,
    group = "school", unit = "race",
    weight = "n", method = "mrc"
)
} # }
```
