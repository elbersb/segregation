<!-- README.md is generated from README.Rmd. Please edit that file -->
mutual
======

An R package to calculate the Mutual Information Index, a segregation index with desirable properties.

Example
-------

The package provides an easy way to calculate total and local segregation, based on the Mutual Information Index.

``` r
library(mutual)
mutual_total(usschools, 'school', 'race', weight = 'n')
#> 0.4215 segegration of unit <school> by group <race>
#> $M
#> [1] 0.4215239
```

Standard errors can be estimated via boostrapping:

``` r
mutual_total(usschools, 'school', 'race', weight = 'n', se = TRUE)
#> ..........
#> 0.4256 (0) segegration of unit <school> by group <race>
#> $M
#> [1] 0.4255889
#> 
#> $se
#> [1] 7.859918e-07
```

Local segregration of racial groups:

``` r
mutual_local(usschools, 'school', 'race', weight = 'n')
#> 0.4215 segegration of unit <school> by group <race>
#>     race        ls           p    M_group
#> 1 native 1.3478746 0.007511419 0.01012445
#> 2  asian 0.5822905 0.022689250 0.01321173
#> 3   hisp 0.7720662 0.151558355 0.11701309
#> 4  black 0.8790017 0.189394665 0.16647824
#> 5  white 0.1823918 0.628846312 0.11469638
```

How to install
--------------

The package is not CRAN yet. If you have devtools installed, use

``` r
devtools::install_github("elbersb/mutual") 
```

to install the package.

To access the documentation, type

``` r
?mutual
```

Papers using the Mutual information index
-----------------------------------------

(list incomplete)

-   DiPrete, T. A., Eller, C. C., Bol, T., & van de Werfhorst, H. G. (2017). School-to-Work Linkages in the United States, Germany, and France. American Journal of Sociology, 122(6), 1869-1938. <https://doi.org/10.1086/691327>
-   Forster, A. G., & Bol, T. (2017). Vocational education and employment over the life course using a new measure of occupational specificity. Social Science Research, 70, 176-197. <https://doi.org/10.1016/j.ssresearch.2017.11.004>
-   Van Puyenbroeck, T., De Bruyne, K., & Sels, L. (2012). More than ‘Mutual Information’: Educational and sectoral gender segregation and their interaction on the Flemish labor market. Labour Economics, 19(1), 1-8. <https://doi.org/10.1016/j.labeco.2011.05.002>
-   Mora, R., & Ruiz-Castillo, J. (2003). Additively decomposable segregation indexes. The case of gender segregation by occupations and human capital levels in Spain. The Journal of Economic Inequality, 1(2), 147-179. <https://doi.org/10.1023/A:1026198429377>

References
----------

Theil, Henri 1971. Principles of Econometrics. New York: Wiley.

Frankel, D. M., & Volij, O. (2011). Measuring school segregation. Journal of Economic Theory, 146(1), 1-38. <https://doi.org/10.1016/j.jet.2010.10.008>

Mora, R., & Ruiz-Castillo, J. (2011). Entropy-based Segregation Indices. Sociological Methodology, 41(1), 159–194. <https://doi.org/10.1111/j.1467-9531.2011.01237.x>
