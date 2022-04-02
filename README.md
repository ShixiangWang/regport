
<!-- README.md is generated from README.Rmd. Please edit that file -->

# regport

<!-- badges: start -->
<!-- badges: end -->

The goal of regport is to provides R6 classes, methods and utilities to
construct, analyze, summarize, and visualize regression models (CoxPH
and GLMs).

## Installation

You can install the development version of regport like so:

``` r
remotes::install_github("ShixiangWang/regport")
```

## Simple case

This is a basic example which shows you how to build and visualize a Cox
model.

Prepare data:

``` r
library(regport)
library(survival)

lung = survival::lung
lung$sex = factor(lung$sex)
```

Create a model:

``` r
model = REGModel$new(
  lung,
  recipe = list(
    x = c("age", "sex"),
    y = c("time", "status")
  )
)
#> Registered S3 method overwritten by 'parameters':
#>   method                         from      
#>   format.parameters_distribution datawizard

model
#> <REGModel>    ========== 
#> 
#> Parameter | Coefficient |       SE |       95% CI |     z |     p
#> -----------------------------------------------------------------
#> age       |        1.02 | 9.38e-03 | [1.00, 1.04] |  1.85 | 0.065
#> sex [2]   |        0.60 |     0.10 | [0.43, 0.83] | -3.06 | 0.002
#> 
#> Uncertainty intervals (equal-tailed) and p values (two-tailed) computed using a
#>   Wald z-distribution approximation.
#> [coxph] model ==========
```

You can also create it with formula:

``` r
model = REGModel$new(
  lung,
  recipe = Surv(time, status) ~ age + sex
)

model
#> <REGModel>    ========== 
#> 
#> Parameter | Coefficient |       SE |       95% CI |     z |     p
#> -----------------------------------------------------------------
#> age       |        1.02 | 9.38e-03 | [1.00, 1.04] |  1.85 | 0.065
#> sex2      |        0.60 |     0.10 | [0.43, 0.83] | -3.06 | 0.002
#> 
#> Uncertainty intervals (equal-tailed) and p values (two-tailed) computed using a
#>   Wald z-distribution approximation.
#> [coxph] model ==========
```

Visualize it:

``` r
model$plot()
```

<img src="man/figures/README-unnamed-chunk-5-1.png" width="100%" />

Visualize with more nice forest plot.

``` r
model$get_forest_data()
#>    variable term term_label   class level level_no   n estimate          SE
#> 1:      age  age        age numeric  <NA>       NA 228 1.017191 0.009381835
#> 2:      sex sex1        sex  factor     1        1 138 0.000000          NA
#> 3:     <NA> sex2        sex  factor     2        2  90 0.598566 0.100234639
#>      CI    CI_low   CI_high         z df_error           p reference label
#> 1: 0.95 0.9989686 1.0357467  1.848078      Inf 0.064591012     FALSE   age
#> 2:   NA        NA        NA        NA       NA          NA      TRUE   sex
#> 3: 0.95 0.4310936 0.8310985 -3.064760      Inf 0.002178445     FALSE  <NA>
model$plot_forest()
```

<img src="man/figures/README-unnamed-chunk-6-1.png" width="100%" />
