# R package RandodiStats
Random distributions and their statistics

## Installation

Install via "devtools"

```
library(devtools)

devtools::install_github("MSeidelFed/RandodiStats_package")

library(RandodiStats_package)
```

## Documentation of usage

```
?RandodiStats::distribution_test_mat()

?RandodiStats::testing_distributions()

?RandodiStats::Variables2Shapes()

?RandodiStats::plotting_distributions()
```

## Needed objects

All our functions take as input a matrix with the same format, to check the format of the input matrix use the "distribution_test_mat()" function:

```
test_mat <- distribution_test_mat()
```
