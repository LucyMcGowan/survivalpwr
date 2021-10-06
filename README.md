
<!-- README.md is generated from README.Rmd. Please edit that file -->

# survivalpwr

<!-- badges: start -->
<!-- badges: end -->

The goal of survivalpwr is to calculate power / sample size for survival
models.

## Installation

<!-- You can install the released version of survivalpwr from [CRAN](https://CRAN.R-project.org) with: -->
<!-- ``` r -->
<!-- install.packages("survivalpwr") -->
<!-- ``` -->

You can install the development version of survivalpwr with:

``` r
devtools::install_github("LucyMcGowan/survivalpwr")
```

## Example

You can input parameters to compute power:

``` r
library(survivalpwr)
pwr_coxph(
  hr = 1.2,
  eventprob = 0.8,
  n = 100)
#> 
#>      Cox Regression power calculation 
#> 
#>               n = 100
#>         nevents = 80
#>              hr = 1.2
#>       eventprob = 0.8
#>         rsquare = 0
#>       sig_level = 0.05
#>           power = 0.1289453
#>     alternative = two.sided
```

Or alternatively, you can enter a target power to determine the sample
size / number of events needed to obtain that power:

``` r
pwr_coxph(
  hr = 1.5,
  eventprob = 0.8,
  power = 0.8
)
#> 
#>      Cox Regression power calculation 
#> 
#>               n = 238.7095
#>         nevents = 190.9676
#>              hr = 1.5
#>       eventprob = 0.8
#>         rsquare = 0
#>       sig_level = 0.05
#>           power = 0.8
#>     alternative = two.sided
```
