# Adjustment of marginal distributions using iterative proportional fitting

Adjusts the marginal distributions for `group` and `unit` in `source` to
the respective marginal distributions in `target`, using the iterative
proportional fitting algorithm (IPF).

## Usage

``` r
ipf(
  source,
  target,
  group,
  unit,
  weight = NULL,
  max_iterations = 100,
  precision = 1e-07
)
```

## Arguments

- source:

  A "source" data frame. The marginals of this dataset are adjusted to
  the marginals of `target`.

- target:

  A "target" data frame. The function returns a dataset where the
  marginal distributions of `group` and `unit` categories are
  approximated by those of `target`.

- group:

  A categorical variable or a vector of variables contained in `source`
  and `target`. Defines the first distribution for adjustment.

- unit:

  A categorical variable or a vector of variables contained in `source`
  and `target`. Defines the second distribution for adjustment.

- weight:

  Numeric. (Default `NULL`)

- max_iterations:

  Maximum number of iterations used for the IPF algorithm.

- precision:

  Convergence criterion for the IPF algorithm. In every iteration, the
  ratio of the source and target marginals are calculated for every
  category of `group` and `unit`. The algorithm converges when all
  ratios are smaller than `1 + precision`.

## Value

Returns a data frame that retains the association structure of `source`
while approximating the marginal distributions for `group` and `unit` of
`target`. The dataset identifies each combination of `group` and `unit`,
and categories that only occur in either `source` or `target` are
dropped. The adjusted frequency of each combination is given by the
column `n`, while `n_target` and `n_source` contain the zero-adjusted
frequencies in the target and source dataset, respectively.

## Details

The algorithm works by scaling the marginal distribution of `group` in
the `source` data frame towards the marginal distribution of `target`;
then repeating this process for `unit`. The algorithm then keeps
alternating between `group` and `unit` until the marginals of the
adjusted data frame are within the allowed precision. This results in a
dataset that retains the association structure of `source` while
approximating the marginal distribution of `target`. If the number of
`unit` and `group` categories is different in `source` and `target`, the
data frame returns the combination of `unit` and `group` categories that
occur in both datasets. Zero values are replaced by a small, non-zero
number (1e-4). Note that the values returned sum to the observations of
the source data frame, not the target data frame. This is different from
other IPF implementations, but ensures that the IPF does not change the
number of observations.

## References

W. E. Deming and F. F. Stephan. 1940. "On a Least Squares Adjustment of
a Sampled Frequency Table When the Expected Marginal Totals are Known".
Annals of Mathematical Statistics. 11 (4): 427–444.

T. Karmel and M. Maclachlan. 1988. "Occupational Sex Segregation —
Increasing or Decreasing?" Economic Record 64: 187-195.

## Examples

``` r
if (FALSE) { # \dontrun{
# adjusts the marginals of group and unit categories so that
# schools00 has similar marginals as schools05
adj <- ipf(schools00, schools05, "race", "school", weight = "n")

# check that the new "race" marginals are similar to the target marginals
# (the same could be done for schools)
aggregate(adj$n, list(adj$race), sum)
aggregate(adj$n_target, list(adj$race), sum)

# note that the adjusted dataset contains fewer
# schools than either the source or the target dataset,
# because the marginals are only defined for the overlap
# of schools
length(unique(schools00$school))
length(unique(schools05$school))
length(unique(adj$school))
} # }
```
