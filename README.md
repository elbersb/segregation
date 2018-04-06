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
#> $M
#> [1] 0.4215239
#> 
#> $bounds
#> [1] 0.000000 1.609438
```

Standard errors can be estimated via boostrapping:

``` r
mutual_total(usschools, "school", "race", weight = "n", se = TRUE)
#> ..........
#> $M
#> [1] 0.4255106
#> 
#> $se
#> [1] 0.0007533536
#> 
#> $bounds
#> [1] 0.000000 1.609438
```

Local segregation of racial groups, with group-specific standard errors:

``` r
mutual_local(usschools, "school", "race", weight = "n", se = TRUE)
#> ..........
#>     race        ls        ls_se           p    M_group   M_group_se
#> 1 native 1.4436245 0.0180736219 0.007544469 0.01083071 0.0001080501
#> 2  asian 0.6219305 0.0044934643 0.022693809 0.01414213 0.0001944925
#> 3   hisp 0.7784047 0.0024908422 0.151554936 0.11796737 0.0003087502
#> 4  black 0.8842662 0.0021405881 0.189574729 0.16743601 0.0004716178
#> 5  white 0.1831239 0.0006878947 0.628632058 0.11515859 0.0003661788
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
-   estimate standard errors via bootstrapping
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
