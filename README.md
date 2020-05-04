# R package RandodiStats
Random distributions and their statistics

Install via "devtools"

```
library(devtools)

devtools::install_github("MSeidelFed/RandodiStats")

library(RandodiStats)
```

## documentation of the functions

```
?RandodiStats::distribution_test_mat()

?RandodiStats::testing_distributions()

?RandodiStats::Variables2Shapes()

?RandodiStats::plotting_distributions()
```

## needed objects

All our functions take as input a matrix with the same format, to check the format of the input matrix use the "distribution_test_mat()" function:

```
test_mat <- distribution_test_mat()
```
