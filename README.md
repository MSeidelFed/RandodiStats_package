# R package RandodiStats
Random distributions and their statistics

## Installation

Install via "devtools"

```{r}
library(devtools)

devtools::install_github("MSeidelFed/RandodiStats_package")

library(RandodiStats_package)
```

## Documentation of usage

```{r}
?RandodiStats::distribution_test_mat()

?RandodiStats::testing_distributions()

?RandodiStats::Variables2Shapes()

?RandodiStats::plotting_distributions()
```

## Needed objects

All our functions take as input a matrix with the same format, to check the format of the input matrix use the "distribution_test_mat()" function:

```{r}
test_mat <- distribution_test_mat()
```

## Dependencies


```{r}
library(fitdistrplus)
library(raster)
library(lawstat)
```
