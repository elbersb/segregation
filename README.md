<!-- README.md is generated from README.Rmd. Please edit that file -->
segregation
===========

An R package to calculate entropy-based segregation indices, with a
focus on the Mutual Information Index (M).

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

Example
-------

The package provides an easy way to calculate total and local
segregation, based on the Mutual Information Index.

``` r
library(segregation)
# example dataset with fake data provided by the package
mutual_total(schools00, "school", "race", weight = "n")
#> # A tibble: 3 x 2
#>   stat    est
#> * <chr> <dbl>
#> 1 M     0.426
#> 2 M_min 0.   
#> 3 M_max 1.61
```

Standard errors can be estimated via boostrapping:

``` r
mutual_total(schools00, "school", "race", weight = "n", se = TRUE)
#> ..........
#> # A tibble: 3 x 3
#>   stat    est       se
#> * <chr> <dbl>    <dbl>
#> 1 M     0.429 0.000935
#> 2 M_min 0.    0.      
#> 3 M_max 1.61  0.
```

Local segregation (`ls`) of racial groups, with group-specific standard
errors:

``` r
mutual_local(schools00, "school", "race", weight = "n", se = TRUE)
#> ..........
#> # A tibble: 15 x 4
#>    race   stat        est       se
#>    <fct>  <fct>     <dbl>    <dbl>
#>  1 asian  ls      0.667   0.00674 
#>  2 black  ls      0.885   0.00259 
#>  3 hisp   ls      0.782   0.00258 
#>  4 white  ls      0.184   0.000725
#>  5 native ls      1.53    0.0229  
#>  6 asian  p       0.0226  0.000124
#>  7 black  p       0.190   0.000465
#>  8 hisp   p       0.152   0.000317
#>  9 white  p       0.628   0.000687
#> 10 native p       0.00745 0.000135
#> 11 asian  M_group 0.0151  0.000193
#> 12 black  M_group 0.168   0.000354
#> 13 hisp   M_group 0.119   0.000336
#> 14 white  M_group 0.116   0.000357
#> 15 native M_group 0.0114  0.000101
```

Decompose the difference in segregation between 2000 and 2005, using the
method developed by Mora and Ruiz-Castillo (2009):

``` r
mutual_difference(schools00, schools05, "school", "race", 
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

The package is not on CRAN yet. If you have devtools installed, use

``` r
devtools::install_github("elbersb/segregation") 
```

to install the package.

To access the documentation, type

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
