<!-- README.md is generated from README.Rmd. Please edit that file -->
segregation
===========

An R package to calculate entropy-based segregation indices, with a focus on the mututal information index (M).

Example
-------

The package provides an easy way to calculate total and local segregation, based on the Mutual Information Index.

``` r
library(segregation)
mutual_total(usschools, "school", "race", weight = "n")
#>        stat   est
#> M         M 0.422
#> M_min M_min 0.000
#> M_max M_max 1.609
```

Standard errors can be estimated via boostrapping:

``` r
mutual_total(usschools, "school", "race", weight = "n", se = TRUE)
#> ..........
#>        stat   est       sd
#> M         M 0.426 0.000753
#> M_min M_min 0.000 0.000000
#> M_max M_max 1.609 0.000000
```

Local segregation of racial groups, with group-specific standard errors:

``` r
mutual_local(usschools, "school", "race", weight = "n", se = TRUE)
#> ..........
#>      race    stat    est        sd
#> 1  native      ls 1.4436 0.0180736
#> 2   asian      ls 0.6219 0.0044935
#> 3    hisp      ls 0.7784 0.0024908
#> 4   black      ls 0.8843 0.0021406
#> 5   white      ls 0.1831 0.0006879
#> 6  native       p 0.0075 0.0000631
#> 7   asian       p 0.0227 0.0001692
#> 8    hisp       p 0.1516 0.0002587
#> 9   black       p 0.1894 0.0005151
#> 10  white       p 0.6289 0.0005570
#> 11 native M_group 0.0108 0.0001081
#> 12  asian M_group 0.0141 0.0001945
#> 13   hisp M_group 0.1180 0.0003088
#> 14  black M_group 0.1674 0.0004716
#> 15  white M_group 0.1152 0.0003662
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

Features
--------

-   calculate total, between, within, and local segregation
-   decompose changes in total segregation over time
-   estimate standard errors via bootstrapping
-   every method returns a [tidy](https://cran.r-project.org/web/packages/tidyr/vignettes/tidy-data.html) data frame for easy post-processing and plotting
-   it's fast, because it uses the [`data.table`](https://github.com/Rdatatable/data.table/wiki) package internally

Papers using the Mutual information index
-----------------------------------------

(list incomplete)

-   DiPrete, T. A., Eller, C. C., Bol, T., & van de Werfhorst, H. G. (2017). School-to-Work Linkages in the United States, Germany, and France. American Journal of Sociology, 122(6), 1869-1938. <https://doi.org/10.1086/691327>
-   Forster, A. G., & Bol, T. (2017). Vocational education and employment over the life course using a new measure of occupational specificity. Social Science Research, 70, 176-197. <https://doi.org/10.1016/j.ssresearch.2017.11.004>
-   Van Puyenbroeck, T., De Bruyne, K., & Sels, L. (2012). More than ‘Mutual Information’: Educational and sectoral gender segregation and their interaction on the Flemish labor market. Labour Economics, 19(1), 1-8. <https://doi.org/10.1016/j.labeco.2011.05.002>
-   Mora, R., & Ruiz-Castillo, J. (2003). Additively decomposable segregation indexes. The case of gender segregation by occupations and human capital levels in Spain. The Journal of Economic Inequality, 1(2), 147-179. <https://doi.org/10.1023/A:1026198429377>

References on entropy-based segregation indices
-----------------------------------------------

Theil, Henri. (1971). Principles of Econometrics. New York: Wiley.

Frankel, D. M., & Volij, O. (2011). Measuring school segregation. Journal of Economic Theory, 146(1), 1-38. <https://doi.org/10.1016/j.jet.2010.10.008>

Mora, R., & Ruiz-Castillo, J. (2011). Entropy-based Segregation Indices. Sociological Methodology, 41(1), 159–194. <https://doi.org/10.1111/j.1467-9531.2011.01237.x>
