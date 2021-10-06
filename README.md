
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

You can input parameters to compute power, for example here is a study
with a hazard ratio of 1.2, an event probability of 0.8, and 200
participants:

``` r
library(survivalpwr)
pwr_coxph(
  hr = 1.2,
  eventprob = 0.8,
  n = 200)
#> 
#>      Cox Regression power calculation 
#> 
#>               n = 200
#>         nevents = 160
#>              hr = 1.2
#>       eventprob = 0.8
#>         rsquare = 0
#>          stddev = 0.5
#>       sig_level = 0.05
#>           power = 0.210799
#>     alternative = two.sided
```

This indicates that this study is 12.9% powered to detect a hazard ratio
of 1.2 with a two-sided test with a significance level of 0.05.

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
#>          stddev = 0.5
#>       sig_level = 0.05
#>           power = 0.8
#>     alternative = two.sided
```

This indicates that you would need 239 observations with an event
probability of 0.8 (or alternatively 191 events total) to be 80% powered
to detect a hazard ratio of 1.5 with a two-sided test with a
significance level of 0.05.

This function also has the ability to incorporate r-squared if you Cox
regression model is expected to have covariates that are explain some
variation in the predictor of interest. For example, if you want to
adjust for covariates that explain 15% of the variation in the predictor
of interest:

``` r
pwr_coxph(
  hr = 1.5,
  eventprob = 0.8,
  power = 0.8,
  rsquare = 0.15
)
#> 
#>      Cox Regression power calculation 
#> 
#>               n = 280.8347
#>         nevents = 224.6677
#>              hr = 1.5
#>       eventprob = 0.8
#>         rsquare = 0.15
#>          stddev = 0.5
#>       sig_level = 0.05
#>           power = 0.8
#>     alternative = two.sided
```

You would now need 281 observations with an event probability of 0.8 (or
225 events).
