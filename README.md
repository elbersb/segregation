<!-- README.md is generated from README.Rmd. Please edit that file -->
segregation
===========

An R package to calculate entropy-based segregation indices, with a focus on the Mutual Information Index (M).

-   calculate total, between, within, and local segregation
-   decompose differences in total segregation over time
-   estimate standard errors via bootstrapping
-   every method returns a [tidy](https://cran.r-project.org/web/packages/tidyr/vignettes/tidy-data.html) data frame for easy post-processing and plotting
-   it's fast, because it uses the [`data.table`](https://github.com/Rdatatable/data.table/wiki) package internally

Example
-------

The package provides an easy way to calculate total and local segregation, based on the Mutual Information Index.

``` r
library(segregation)
# the dataset contains information on the racial composition of schools in three U.S. states
mutual_total(usschools00, "school", "race", weight = "n")
#>        stat   est
#> M         M 0.422
#> M_min M_min 0.000
#> M_max M_max 1.609
```

Standard errors can be estimated via boostrapping:

``` r
mutual_total(usschools00, "school", "race", weight = "n", se = TRUE)
#> ..........
#>        stat   est       se
#> M         M 0.425 0.000591
#> M_min M_min 0.000 0.000000
#> M_max M_max 1.609 0.000000
```

Local segregation (`ls`) of racial groups, with group-specific standard errors:

``` r
mutual_local(usschools00, "school", "race", weight = "n", se = TRUE)
#> ..........
#>      race    stat     est        se
#> 1  native      ls 1.44064 0.0179202
#> 2   asian      ls 0.62401 0.0076724
#> 3    hisp      ls 0.77865 0.0013172
#> 4   black      ls 0.88337 0.0021738
#> 5   white      ls 0.18261 0.0006489
#> 6  native       p 0.00751 0.0000904
#> 7   asian       p 0.02271 0.0001572
#> 8    hisp       p 0.15149 0.0003427
#> 9   black       p 0.18922 0.0004567
#> 10  white       p 0.62908 0.0006681
#> 11 native M_group 0.01082 0.0001129
#> 12  asian M_group 0.01417 0.0001352
#> 13   hisp M_group 0.11796 0.0003884
#> 14  black M_group 0.16715 0.0004115
#> 15  white M_group 0.11487 0.0003110
```

Decompose the difference in segregation between 2000 and 2005, using the method developed by Mora and Ruiz-Castillo (2009):

``` r
mutual_difference(usschools00, usschools05, "school", "race", 
                  weight = "n", method = "mrc")
#>                          stat      est
#> M1                         M1  0.42152
#> M2                         M2  0.41045
#> diff                     diff -0.01107
#> group_marginal group_marginal  0.00761
#> unit_entropy     unit_entropy -0.06375
#> invariant           invariant  0.04506
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

-   DiPrete, T. A., Eller, C. C., Bol, T., & van de Werfhorst, H. G. (2017). School-to-Work Linkages in the United States, Germany, and France. American Journal of Sociology, 122(6), 1869-1938. <https://doi.org/10.1086/691327>
-   Forster, A. G., & Bol, T. (2017). Vocational education and employment over the life course using a new measure of occupational specificity. Social Science Research, 70, 176-197. <https://doi.org/10.1016/j.ssresearch.2017.11.004>
-   Van Puyenbroeck, T., De Bruyne, K., & Sels, L. (2012). More than ‘Mutual Information’: Educational and sectoral gender segregation and their interaction on the Flemish labor market. Labour Economics, 19(1), 1-8. <https://doi.org/10.1016/j.labeco.2011.05.002>
-   Mora, R., & Ruiz-Castillo, J. (2003). Additively decomposable segregation indexes. The case of gender segregation by occupations and human capital levels in Spain. The Journal of Economic Inequality, 1(2), 147-179. <https://doi.org/10.1023/A:1026198429377>

References on entropy-based segregation indices
-----------------------------------------------

Theil, Henri. (1971). Principles of Econometrics. New York: Wiley.

Frankel, D. M., & Volij, O. (2011). Measuring school segregation. Journal of Economic Theory, 146(1), 1-38. <https://doi.org/10.1016/j.jet.2010.10.008>

Mora, R., & Ruiz-Castillo, J. (2009). The Invariance Properties of the Mutual Information Index of Multigroup Segregation. Research on Economic Inequality, 17, 33-53.

Mora, R., & Ruiz-Castillo, J. (2011). Entropy-based Segregation Indices. Sociological Methodology, 41(1), 159–194. <https://doi.org/10.1111/j.1467-9531.2011.01237.x>
