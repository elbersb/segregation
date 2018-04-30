<!-- README.md is generated from README.Rmd. Please edit that file -->
segregation
===========

[![CRAN
Version](https://www.r-pkg.org/badges/version/segregation)](https://CRAN.R-project.org/package=segregation)
[![Build
Status](https://travis-ci.org/elbersb/segregation.svg?branch=master)](https://travis-ci.org/elbersb/segregation)
[![codecov](https://codecov.io/gh/elbersb/segregation/branch/master/graph/badge.svg)](https://codecov.io/gh/elbersb/segregation)

An R package to calculate and decompose entropy-based, multigroup
segregation indices, with a focus on the Mutual Information Index (M)
and Theil’s Information Index (H).

-   calculate total, between, within, and local segregation
-   decompose differences in total segregation over time
-   estimate standard errors via bootstrapping
-   every method returns a
    [tidy](http://vita.had.co.nz/papers/tidy-data.html) data frame (or
    [tibble](http://tibble.tidyverse.org), if the package is installed)
    for easy post-processing and plotting
-   it’s fast, because it uses the
    [`data.table`](https://github.com/Rdatatable/data.table/wiki)
    package internally

Usage
-----

The package provides an easy way to calculate segregation measures,
based on the Mutual Information Index (M) and Theil’s Entropy Index (H).

``` r
library(segregation)
# example dataset with fake data provided by the package
mutual_total(schools00, "race", "school", weight = "n")
#> # A tibble: 2 x 2
#>   stat    est
#> * <chr> <dbl>
#> 1 M     0.426
#> 2 H     0.419
```

Standard errors in all functions can be estimated via boostrapping:

``` r
mutual_total(schools00, "race", "school", weight = "n", se = TRUE)
#> ..........
#> # A tibble: 2 x 3
#>   stat    est       se
#> * <chr> <dbl>    <dbl>
#> 1 M     0.429 0.000935
#> 2 H     0.422 0.000985
```

Decompose segregation into a between-state and a within-state term (the
sum of these equals total segregation):

``` r
# between states
mutual_total(schools00, "race", "state", weight = "n")
#> # A tibble: 2 x 2
#>   stat     est
#> * <chr>  <dbl>
#> 1 M     0.0992
#> 2 H     0.0977

# within states
mutual_total(schools00, "race", "school", within = "state", weight = "n")
#> # A tibble: 2 x 2
#>   stat    est
#> * <chr> <dbl>
#> 1 M     0.326
#> 2 H     0.321
```

Local segregation (`ls`) is a decomposition by units (here racial
groups). The sum of the proportion-weighted local segregation scores
equals M:

``` r
(local <- mutual_local(schools00, group = "school", unit = "race", weight = "n", 
             se = TRUE, wide = TRUE))
#> ..........
#> # A tibble: 5 x 5
#>   race      ls    ls_se       p     p_se
#>   <fct>  <dbl>    <dbl>   <dbl>    <dbl>
#> 1 asian  0.667 0.00674  0.0226  0.000124
#> 2 black  0.885 0.00259  0.190   0.000465
#> 3 hisp   0.782 0.00258  0.152   0.000317
#> 4 white  0.184 0.000725 0.628   0.000687
#> 5 native 1.53  0.0229   0.00745 0.000135

sum(local$p * local$ls)
#> [1] 0.429
```

Decompose the difference in M between 2000 and 2005, using the method
developed by Mora and Ruiz-Castillo (2009):

``` r
mutual_difference(schools00, schools05, group = "race", unit = "school",
                  weight = "n", method = "mrc")
#> # A tibble: 6 x 2
#>   stat                est
#> * <chr>             <dbl>
#> 1 M1              0.426  
#> 2 M2              0.413  
#> 3 diff           -0.0122 
#> 4 group_marginal  0.00747
#> 5 unit_entropy   -0.0641 
#> 6 invariant       0.0445
```

How to install
--------------

To install the package from CRAN, use

``` r
install.packages("segregation") 
```

To install the development version, use

``` r
devtools::install_github("elbersb/segregation") 
```

To access the documentation
([PDF](https://cran.r-project.org/web/packages/segregation/segregation.pdf)),
type

``` r
?segregation
```

Papers using the Mutual information index
-----------------------------------------

(list incomplete)

-   DiPrete, T. A., Eller, C. C., Bol, T., & van de Werfhorst, H. G.
    (2017). School-to-Work Linkages in the United States, Germany, and
    France. American Journal of Sociology, 122(6), 1869-1938.
    <https://doi.org/10.1086/691327>
-   Forster, A. G., & Bol, T. (2017). Vocational education and
    employment over the life course using a new measure of occupational
    specificity. Social Science Research, 70, 176-197.
    <https://doi.org/10.1016/j.ssresearch.2017.11.004>
-   Van Puyenbroeck, T., De Bruyne, K., & Sels, L. (2012). More than
    ‘Mutual Information’: Educational and sectoral gender segregation
    and their interaction on the Flemish labor market. Labour Economics,
    19(1), 1-8. <https://doi.org/10.1016/j.labeco.2011.05.002>
-   Mora, R., & Ruiz-Castillo, J. (2003). Additively decomposable
    segregation indexes. The case of gender segregation by occupations
    and human capital levels in Spain. The Journal of Economic
    Inequality, 1(2), 147-179. <https://doi.org/10.1023/A:1026198429377>

References on entropy-based segregation indices
-----------------------------------------------

Theil, Henri. (1971). Principles of Econometrics. New York: Wiley.

Frankel, D. M., & Volij, O. (2011). Measuring school segregation.
Journal of Economic Theory, 146(1), 1-38.
<https://doi.org/10.1016/j.jet.2010.10.008>

Mora, R., & Ruiz-Castillo, J. (2009). The Invariance Properties of the
Mutual Information Index of Multigroup Segregation. Research on Economic
Inequality, 17, 33-53.

Mora, R., & Ruiz-Castillo, J. (2011). Entropy-based Segregation Indices.
Sociological Methodology, 41(1), 159–194.
<https://doi.org/10.1111/j.1467-9531.2011.01237.x>
