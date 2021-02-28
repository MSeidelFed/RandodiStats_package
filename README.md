# R package RandodiStats
Random distributions and their statistics

## First step

It is necessary to install [Rtools](https://cran.r-project.org/bin/windows/Rtools/history.html) before starting. Make sure to install the appropriate version depending on your R version and add the Rtools directory to the system PATH (instructions for this can be found [here](https://datag.org/resources/documents/spring-2018/37-de-barros-installing-r-on-windows/file))

## Installation

Install via "devtools"

```{r}
library(devtools)

devtools::install_github("MSeidelFed/RandodiStats_package")

library(RandoDiStats)
```

## Documentation of usage

```{r}
?RandoDiStats::distribution_test_mat()

?RandoDiStats::testing_distributions()

?RandoDiStats::Variables2Shapes()

?RandoDiStats::plotting_distributions()

?RandoDiStats::ClustPlus()

?RandoDiStats::KmeansPlus()

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
library(matrixStats)
library(tidyr)
library(dendextend)
library(pvclust)
library(ComplexHeatmap)
library(circlize)
library(ggplot2)
library(reshape2)
library(ggfortify)
library(plotrix)
```


## Examples

```{r}
### this is how the input matrix should look like, metabolites in columns, treatments in rows (wide data)
test_mat <- distribution_test_mat()


### plot the distributions of your metabolites against common distributions
plotting_distributions(test_mat = distribution_test_mat2())


### test the distributions of your metabolites (this function is implemented into the Omics test to select an appropriate family for regression)
testing_distributions()

### obtain the geometrical shapes of your metabolite distributions (this is already implemented into the plotting and Omics testing functions)
Variables2Shapes()

### this needs to be a factor with the same size as rows in your columns and is meant to cover the treatment/s
Factor1_eg <- as.factor(c(rep("RED", 200), rep("GREEN", 200), rep("BLACK", 200),
                          rep("WHITE", 200), rep("YELLOW", 200)))

test_OUS <- OmicsUnivariateStats(Factor1 = Factor1_eg)

### run in the console, the function is interactive.

test_CP <- ClustPlus()

### k-means

test_Km <- KmeansPlus(DataDir = distribution_test_mat(nrow_x = 20, n_random_distributions = 10), n_boot = 1)

```

