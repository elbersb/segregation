
<!-- README.md is generated from README.Rmd. Please edit that file -->

# segregation

[![CRAN
Version](https://www.r-pkg.org/badges/version/segregation)](https://CRAN.R-project.org/package=segregation)
[![Build
Status](https://travis-ci.org/elbersb/segregation.svg?branch=master)](https://travis-ci.org/elbersb/segregation)
[![codecov](https://codecov.io/gh/elbersb/segregation/branch/master/graph/badge.svg)](https://codecov.io/gh/elbersb/segregation)

An R package to calculate and decompose entropy-based, multigroup
segregation indices, with a focus on the Mutual Information Index (M)
and Theil’s Information Index (H).

  - calculate total, between, within, and local segregation
  - decompose differences in total segregation over time
  - estimate standard errors via bootstrapping
  - every method returns a
    [tidy](http://vita.had.co.nz/papers/tidy-data.html) data frame for
    easy post-processing and plotting
  - it’s fast, because it uses the
    [`data.table`](https://github.com/Rdatatable/data.table/wiki)
    package internally

## Usage

The package provides an easy way to calculate segregation measures,
based on the Mutual Information Index (M) and Theil’s Entropy Index (H).

``` r
library(segregation)

# example dataset with fake data provided by the package
mutual_total(schools00, "race", "school", weight = "n")
#>  stat   est
#>     M 0.426
#>     H 0.419
```

Standard errors in all functions can be estimated via boostrapping:

``` r
mutual_total(schools00, "race", "school", weight = "n", se = TRUE)
#>  stat   est       se
#>     M 0.429 0.000935
#>     H 0.422 0.000985
```

Decompose segregation into a between-state and a within-state term (the
sum of these equals total segregation):

``` r
# between states
mutual_total(schools00, "race", "state", weight = "n")
#>  stat    est
#>     M 0.0992
#>     H 0.0977

# within states
mutual_total(schools00, "race", "school", within = "state", weight = "n")
#>  stat   est
#>     M 0.326
#>     H 0.321
```

Local segregation (`ls`) is a decomposition by units (here racial
groups). The sum of the proportion-weighted local segregation scores
equals
M:

``` r
(local <- mutual_local(schools00, group = "school", unit = "race", weight = "n",
             se = TRUE, wide = TRUE))
#>    race    ls    ls_se       p     p_se
#>   asian 0.667 0.006736 0.02261 0.000124
#>   black 0.885 0.002595 0.19005 0.000465
#>    hisp 0.782 0.002582 0.15179 0.000317
#>   white 0.184 0.000725 0.62810 0.000687
#>  native 1.528 0.022868 0.00745 0.000135

sum(local$p * local$ls)
#> [1] 0.429
```

Decompose the difference in M between 2000 and 2005, using iterative
proportional fitting (IPF) and the Shapley decomposition, as suggested
by Karmel and Maclachlan (1988) and Deutsch et al. (2006):

``` r
mutual_difference(schools00, schools05, group = "race", unit = "school",
                  weight = "n", method = "shapley")
#>            stat      est
#>              M1  0.42554
#>              M2  0.41339
#>            diff -0.01215
#>       additions -0.00341
#>        removals -0.01141
#>  group_marginal  0.01623
#>   unit_marginal -0.01674
#>      structural  0.00318
```

Find more information in the
[documentation](https://elbersb.de/segregation).

## How to install

To install the package from CRAN, use

``` r
install.packages("segregation")
```

To install the development version, use

``` r
devtools::install_github("elbersb/segregation")
```

## Papers using the Mutual information index

(list incomplete)

DiPrete, T. A., Eller, C. C., Bol, T., & van de Werfhorst, H. G. (2017).
School-to-Work Linkages in the United States, Germany, and France.
American Journal of Sociology, 122(6), 1869-1938.
<https://doi.org/10.1086/691327>

Forster, A. G., & Bol, T. (2017). Vocational education and employment
over the life course using a new measure of occupational specificity.
Social Science Research, 70, 176-197.
<https://doi.org/10.1016/j.ssresearch.2017.11.004>

Van Puyenbroeck, T., De Bruyne, K., & Sels, L. (2012). More than ‘Mutual
Information’: Educational and sectoral gender segregation and their
interaction on the Flemish labor market. Labour Economics, 19(1), 1-8.
<https://doi.org/10.1016/j.labeco.2011.05.002>

Mora, R., & Ruiz-Castillo, J. (2003). Additively decomposable
segregation indexes. The case of gender segregation by occupations and
human capital levels in Spain. The Journal of Economic Inequality, 1(2),
147-179. <https://doi.org/10.1023/A:1026198429377>

## References on entropy-based segregation indices

Deutsch, J., Flückiger, Y. & Silber, J. (2009). Analyzing Changes in
Occupational Segregation: The Case of Switzerland (1970–2000), in: Yves
Flückiger, Sean F. Reardon, Jacques Silber (eds.) Occupational and
Residential Segregation (Research on Economic Inequality, Volume 17),
171–202.

Theil, H. (1971). Principles of Econometrics. New York: Wiley.

Frankel, D. M., & Volij, O. (2011). Measuring school segregation.
Journal of Economic Theory, 146(1), 1-38.
<https://doi.org/10.1016/j.jet.2010.10.008>

Mora, R., & Ruiz-Castillo, J. (2009). The Invariance Properties of the
Mutual Information Index of Multigroup Segregation, in: Yves Flückiger,
Sean F. Reardon, Jacques Silber (eds.) Occupational and Residential
Segregation (Research on Economic Inequality, Volume 17), 33-53.

Mora, R., & Ruiz-Castillo, J. (2011). Entropy-based Segregation Indices.
Sociological Methodology, 41(1), 159–194.
<https://doi.org/10.1111/j.1467-9531.2011.01237.x>

Karmel, T. & Maclachlan, M. (1988). Occupational Sex Segregation —
Increasing or Decreasing? Economic Record 64: 187-195.
<https://doi.org/10.1111/j.1475-4932.1988.tb02057.x>

Watts, M. The Use and Abuse of Entropy Based Segregation Indices.
Working Paper. URL:
<http://www.ecineq.org/ecineq_lux15/FILESx2015/CR2/p217.pdf>
