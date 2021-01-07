# segregation 0.4.0

* faster bootstrap
* return bootstrap estimates as attr
* add mutual_expected
* apply bias-correction via bootstrap by default when se=TRUE

# segregation 0.3.0

* always return data.table
* for ipf function, warn when groups/units are dropped
* return sample size of source dataset for IPF
* don't allow bootstrap when sample size is not an integer, but allow non-integer sample weights (which are unproblematic)
* simplify precision parameter for ipf procedure
* increase default bootstrap to 100
* fix data.table issue (#3)

# segregation 0.2.0

* add "shapley" decomposition method, revisit other difference decomposition methods
* better logging of bootstrap/IPF
* several small fixes
* add lintr package
* add warning when attempting bootstrap with non-integer weights

# segregation 0.1.0

* switch group and unit definitions, to be consistent with the literature
* add Theil's Information Index (H)
* add entropy function
* add mutual_within function to decompose weighted within indices
* add "wide" option to mutual_local and mutual_within
* add "ipf" (iterative proportional fitting) function and a difference decomposition based on IPF
* "mrc_adjusted" difference decomposition is defined only on overlap sample of units and groups
* internal refactoring

# segregation 0.0.1

Initial release.

