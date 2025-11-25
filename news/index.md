# Changelog

## segregation (development version)

- ipf: decrease default precision (longer runtime, but more precise
  results)

## segregation 1.1.0

CRAN release: 2023-12-02

- various improvements to compression algorithm
- add dendrogram visualization
- allow multiple curves in `segcurve` function
- segplot: add optional ‘secondary_plot’ argument
- segplot: remove ‘title’ argument
- segplot: add optional ‘hline’ argument

## segregation 1.0.0

CRAN release: 2023-08-24

- add mutual_total_nested
- add within argument to mutual_expected
- add dissimilarity_expected
- add suite of compression-related functions (some in C++)
- add segplot function
- add functions exposure and isolation
- fix that roxygen2 problem

## segregation 0.6.0

CRAN release: 2021-09-02

- faster mutual_total(…, within)
- updated docs
- some minor bug fixes
- some improved error messages

## segregation 0.5.0

CRAN release: 2021-02-08

- dissimilarity: support for the index of dissimilarity
- add CI argument for confidence intervals
- mutual_within: report ent_ratio instead of h_weight
- matrix_to_long: convert contingency tables into long form
- add introductory vignette

## segregation 0.4.0

CRAN release: 2021-01-08

- faster bootstrap
- return bootstrap estimates as attr
- add mutual_expected
- apply bias-correction via bootstrap by default when se=TRUE

## segregation 0.3.0

CRAN release: 2019-09-20

- always return data.table
- for ipf function, warn when groups/units are dropped
- return sample size of source dataset for IPF
- don’t allow bootstrap when sample size is not an integer, but allow
  non-integer sample weights (which are unproblematic)
- simplify precision parameter for ipf procedure
- increase default bootstrap to 100
- fix data.table issue
  ([\#3](https://github.com/elbersb/segregation/issues/3))

## segregation 0.2.0

CRAN release: 2019-01-14

- add “shapley” decomposition method, revisit other difference
  decomposition methods
- better logging of bootstrap/IPF
- several small fixes
- add lintr package
- add warning when attempting bootstrap with non-integer weights

## segregation 0.1.0

CRAN release: 2018-06-15

- switch group and unit definitions, to be consistent with the
  literature
- add Theil’s Information Index (H)
- add entropy function
- add mutual_within function to decompose weighted within indices
- add “wide” option to mutual_local and mutual_within
- add “ipf” (iterative proportional fitting) function and a difference
  decomposition based on IPF
- “mrc_adjusted” difference decomposition is defined only on overlap
  sample of units and groups
- internal refactoring

## segregation 0.0.1

CRAN release: 2018-04-17

Initial release.
