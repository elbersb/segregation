# Package index

## Segregation indices

- [`dissimilarity()`](https://elbersb.com/segregation/reference/dissimilarity.md)
  : Calculates Index of Dissimilarity
- [`exposure()`](https://elbersb.com/segregation/reference/exposure.md)
  : Calculates pairwise exposure indices
- [`isolation()`](https://elbersb.com/segregation/reference/isolation.md)
  : Calculates isolation indices
- [`mutual_total()`](https://elbersb.com/segregation/reference/mutual_total.md)
  : Calculates the Mutual Information Index M and Theil's Entropy Index
  H
- [`mutual_total_nested()`](https://elbersb.com/segregation/reference/mutual_total_nested.md)
  : Calculates a nested decomposition of segregation for M and H
- [`mutual_within()`](https://elbersb.com/segregation/reference/mutual_within.md)
  : Calculates detailed within-category segregation scores for M and H
- [`mutual_local()`](https://elbersb.com/segregation/reference/mutual_local.md)
  : Calculates local segregation scores based on M

## Visualizing segregation

- [`segcurve()`](https://elbersb.com/segregation/reference/segcurve.md)
  : A visual representation of two-group segregation
- [`segplot()`](https://elbersb.com/segregation/reference/segplot.md) :
  A visual representation of segregation

## Debiasing

- [`mutual_expected()`](https://elbersb.com/segregation/reference/mutual_expected.md)
  : Calculates expected values when true segregation is zero
- [`dissimilarity_expected()`](https://elbersb.com/segregation/reference/dissimilarity_expected.md)
  : Calculates expected values when true segregation is zero

## Comparing differences

- [`mutual_difference()`](https://elbersb.com/segregation/reference/mutual_difference.md)
  : Decomposes the difference between two M indices
- [`ipf()`](https://elbersb.com/segregation/reference/ipf.md) :
  Adjustment of marginal distributions using iterative proportional
  fitting

## Compressing segregation

- [`compress()`](https://elbersb.com/segregation/reference/compress.md)
  : Compresses a data matrix based on mutual information (segregation)
- [`merge_units()`](https://elbersb.com/segregation/reference/merge_units.md)
  : Creates a compressed dataset
- [`get_crosswalk()`](https://elbersb.com/segregation/reference/get_crosswalk.md)
  : Create crosswalk after compression
- [`scree_plot()`](https://elbersb.com/segregation/reference/scree_plot.md)
  : Scree plot for segregation compression

## Datasets

- [`school_ses`](https://elbersb.com/segregation/reference/school_ses.md)
  : Student-level data including SES status
- [`schools00`](https://elbersb.com/segregation/reference/schools00.md)
  : Ethnic/racial composition of schools for 2000/2001
- [`schools05`](https://elbersb.com/segregation/reference/schools05.md)
  : Ethnic/racial composition of schools for 2005/2006

## Helper functions

- [`entropy()`](https://elbersb.com/segregation/reference/entropy.md) :
  Calculates the entropy of a distribution
- [`matrix_to_long()`](https://elbersb.com/segregation/reference/matrix_to_long.md)
  : Turns a contingency table into long format
