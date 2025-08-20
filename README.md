
<!-- README.md is generated from README.Rmd. Please edit that file -->

# filtro <a href='https://filtro.tidymodels.org/dev/'><img src='man/figures/logo.png' align="right" height="139" /></a>

<!-- badges: start -->

[![R-CMD-check](https://github.com/tidymodels/filtro/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/tidymodels/filtro/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/tidymodels/filtro/graph/badge.svg)](https://app.codecov.io/gh/tidymodels/filtro)
[![CRAN
status](https://www.r-pkg.org/badges/version/filtro)](https://CRAN.R-project.org/package=filtro)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

> ⚠️ **filtro is under active development; breaking changes may occur.**

## Overview

filtro is tidy tools to apply filter-based supervised feature selection
methods. These methods score and rank feature relevance using metrics
such as p-values, correlation, feature importance, information gain, and
more.

The package provides functions to rank and select a top proportion or
number of features using built-in methods and the
[desirability2](https://desirability2.tidymodels.org) package, and
supports streamlined preprocessing, either standalone or within
tidymodels workflows such as the
[recipes](https://recipes.tidymodels.org) package.

For a detailed introduction, please see
[vignette(“filtro”)](https://filtro.tidymodels.org/dev/articles/filtro.html).

## Installation

Install the released version of filtro from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("filtro")
```

Install the development version from GitHub with:

``` r
# install.packages("pak")
pak::pak("tidymodels/filtro")
```

## Feature selection methods

Currently, the implemented filters include:

1.  ANOVA F-test

2.  Correlation

3.  Random forest feature importance

4.  Information gain

5.  Area under the ROC curve

6.  Cross tabulation (Chi-squared test and Fisher’s exact test)

## Scoring examples

``` r
library(filtro)
library(desirability2)
library(dplyr)
library(modeldata)
```

``` r
ames_subset <- modeldata::ames |>
  # Use a subset of data for demonstration
  dplyr::select(
    Sale_Price,
    MS_SubClass,
    MS_Zoning,
    Lot_Frontage,
    Lot_Area,
    Street
  )
ames_subset <- ames_subset |>
  dplyr::mutate(Sale_Price = log10(Sale_Price))
```

``` r
# ANOVA p-value
ames_aov_pval_res <-
  score_aov_pval |>
  fit(Sale_Price ~ ., data = ames_subset)
ames_aov_pval_res@results
#> # A tibble: 5 × 4
#>   name      score outcome    predictor   
#>   <chr>     <dbl> <chr>      <chr>       
#> 1 aov_pval 237.   Sale_Price MS_SubClass 
#> 2 aov_pval 130.   Sale_Price MS_Zoning   
#> 3 aov_pval  NA    Sale_Price Lot_Frontage
#> 4 aov_pval  NA    Sale_Price Lot_Area    
#> 5 aov_pval   5.75 Sale_Price Street
```

``` r
# Pearson correlation
ames_cor_pearson_res <-
  score_cor_pearson |>
  fit(Sale_Price ~ ., data = ames_subset)
ames_cor_pearson_res@results
#> # A tibble: 5 × 4
#>   name         score outcome    predictor   
#>   <chr>        <dbl> <chr>      <chr>       
#> 1 cor_pearson NA     Sale_Price MS_SubClass 
#> 2 cor_pearson NA     Sale_Price MS_Zoning   
#> 3 cor_pearson  0.165 Sale_Price Lot_Frontage
#> 4 cor_pearson  0.255 Sale_Price Lot_Area    
#> 5 cor_pearson NA     Sale_Price Street
```

``` r
# Forest importance
ames_imp_rf_reg_res <-
  score_imp_rf |>
  fit(Sale_Price ~ ., data = ames_subset, seed = 42)
ames_imp_rf_reg_res@results
#> # A tibble: 5 × 4
#>   name       score outcome    predictor   
#>   <chr>      <dbl> <chr>      <chr>       
#> 1 imp_rf 0.0144    Sale_Price MS_SubClass 
#> 2 imp_rf 0.0102    Sale_Price MS_Zoning   
#> 3 imp_rf 0.00693   Sale_Price Lot_Frontage
#> 4 imp_rf 0.0144    Sale_Price Lot_Area    
#> 5 imp_rf 0.0000308 Sale_Price Street
```

``` r
# Information gain
ames_info_gain_reg_res <-
  score_info_gain |>
  fit(Sale_Price ~ ., data = ames_subset)
ames_info_gain_reg_res@results
#> # A tibble: 5 × 4
#>   name       score outcome    predictor   
#>   <chr>      <dbl> <chr>      <chr>       
#> 1 infogain 0.266   Sale_Price MS_SubClass 
#> 2 infogain 0.113   Sale_Price MS_Zoning   
#> 3 infogain 0.146   Sale_Price Lot_Frontage
#> 4 infogain 0.140   Sale_Price Lot_Area    
#> 5 infogain 0.00365 Sale_Price Street
```

## Filtering exmples for score *singular*

``` r
ames_aov_pval_res@results
#> # A tibble: 5 × 4
#>   name      score outcome    predictor   
#>   <chr>     <dbl> <chr>      <chr>       
#> 1 aov_pval 237.   Sale_Price MS_SubClass 
#> 2 aov_pval 130.   Sale_Price MS_Zoning   
#> 3 aov_pval  NA    Sale_Price Lot_Frontage
#> 4 aov_pval  NA    Sale_Price Lot_Area    
#> 5 aov_pval   5.75 Sale_Price Street
```

``` r
# Show best score, based on proportion of predictors
ames_aov_pval_res |> show_best_score_prop(prop_terms = 0.2)
#> # A tibble: 1 × 4
#>   name     score outcome    predictor  
#>   <chr>    <dbl> <chr>      <chr>      
#> 1 aov_pval  237. Sale_Price MS_SubClass
```

``` r
# Fill safe value, then show best score 
ames_aov_pval_res <- ames_aov_pval_res |> fill_safe_value()
ames_aov_pval_res |> show_best_score_prop(prop_terms = 0.2)
#> # A tibble: 2 × 4
#>   name     score outcome    predictor   
#>   <chr>    <dbl> <chr>      <chr>       
#> 1 aov_pval   Inf Sale_Price Lot_Frontage
#> 2 aov_pval   Inf Sale_Price Lot_Area
```

## Filtering examples for scores *plural*

``` r
# Create a list
class_score_list <- list(
  ames_cor_pearson_res,
  ames_imp_rf_reg_res,
  ames_info_gain_reg_res
)
```

``` r
# Fill safe values
ames_scores_results <- class_score_list |>
  fill_safe_values() |>
  # Remove outcome
  dplyr::select(-outcome)
ames_scores_results
#> # A tibble: 5 × 4
#>   predictor    cor_pearson    imp_rf infogain
#>   <chr>              <dbl>     <dbl>    <dbl>
#> 1 MS_SubClass        1     0.0144     0.266  
#> 2 MS_Zoning          1     0.0102     0.113  
#> 3 Lot_Frontage       0.165 0.00693    0.146  
#> 4 Lot_Area           0.255 0.0144     0.140  
#> 5 Street             1     0.0000308  0.00365
```

``` r
# Single and multi-parameter optimization using desirability functions
# Optimize correlation alone
ames_scores_results |>
  show_best_desirability_prop(
    maximize(cor_pearson, low = 0, high = 1)
  )
#> # A tibble: 5 × 6
#>   predictor    cor_pearson    imp_rf infogain .d_max_cor_pearson .d_overall
#>   <chr>              <dbl>     <dbl>    <dbl>              <dbl>      <dbl>
#> 1 MS_SubClass        1     0.0144     0.266                1          1    
#> 2 MS_Zoning          1     0.0102     0.113                1          1    
#> 3 Street             1     0.0000308  0.00365              1          1    
#> 4 Lot_Area           0.255 0.0144     0.140                0.255      0.255
#> 5 Lot_Frontage       0.165 0.00693    0.146                0.165      0.165

# Optimize correlation and forest importance
ames_scores_results |>
  show_best_desirability_prop(
    maximize(cor_pearson, low = 0, high = 1),
    maximize(imp_rf)
  )
#> # A tibble: 5 × 7
#>   predictor    cor_pearson    imp_rf infogain .d_max_cor_pearson .d_max_imp_rf
#>   <chr>              <dbl>     <dbl>    <dbl>              <dbl>         <dbl>
#> 1 MS_SubClass        1     0.0144     0.266                1             1    
#> 2 MS_Zoning          1     0.0102     0.113                1             0.705
#> 3 Lot_Area           0.255 0.0144     0.140                0.255         0.994
#> 4 Lot_Frontage       0.165 0.00693    0.146                0.165         0.479
#> 5 Street             1     0.0000308  0.00365              1             0    
#> # ℹ 1 more variable: .d_overall <dbl>

# Optimize correlation, forest importance and information gain
ames_scores_results |>
  show_best_desirability_prop(
    maximize(cor_pearson, low = 0, high = 1),
    maximize(imp_rf),
    maximize(infogain)
  )
#> # A tibble: 5 × 8
#>   predictor    cor_pearson    imp_rf infogain .d_max_cor_pearson .d_max_imp_rf
#>   <chr>              <dbl>     <dbl>    <dbl>              <dbl>         <dbl>
#> 1 MS_SubClass        1     0.0144     0.266                1             1    
#> 2 MS_Zoning          1     0.0102     0.113                1             0.705
#> 3 Lot_Area           0.255 0.0144     0.140                0.255         0.994
#> 4 Lot_Frontage       0.165 0.00693    0.146                0.165         0.479
#> 5 Street             1     0.0000308  0.00365              1             0    
#> # ℹ 2 more variables: .d_max_infogain <dbl>, .d_overall <dbl>

# Same as above, but retain only a proportion of predictors
ames_scores_results |>
  show_best_desirability_prop(
    maximize(cor_pearson, low = 0, high = 1),
    maximize(imp_rf),
    maximize(infogain),
    prop_terms = 0.2
  )
#> # A tibble: 1 × 8
#>   predictor   cor_pearson imp_rf infogain .d_max_cor_pearson .d_max_imp_rf
#>   <chr>             <dbl>  <dbl>    <dbl>              <dbl>         <dbl>
#> 1 MS_SubClass           1 0.0144    0.266                  1             1
#> # ℹ 2 more variables: .d_max_infogain <dbl>, .d_overall <dbl>

# Optimize toward a target
ames_scores_results |>
  show_best_desirability_prop(
    target(cor_pearson, low = 0.2, target = 0.255, high = 0.9)
  )
#> # A tibble: 5 × 6
#>   predictor    cor_pearson    imp_rf infogain .d_target_cor_pearson .d_overall
#>   <chr>              <dbl>     <dbl>    <dbl>                 <dbl>      <dbl>
#> 1 Lot_Area           0.255 0.0144     0.140                    1.00       1.00
#> 2 MS_SubClass        1     0.0144     0.266                    0          0   
#> 3 MS_Zoning          1     0.0102     0.113                    0          0   
#> 4 Lot_Frontage       0.165 0.00693    0.146                    0          0   
#> 5 Street             1     0.0000308  0.00365                  0          0

# Optimize with box constraints
ames_scores_results |>
  show_best_desirability_prop(
    constrain(cor_pearson, low = 0.2, high = 1)
  )
#> # A tibble: 5 × 6
#>   predictor    cor_pearson    imp_rf infogain .d_box_cor_pearson .d_overall
#>   <chr>              <dbl>     <dbl>    <dbl>              <dbl>      <dbl>
#> 1 MS_SubClass        1     0.0144     0.266                    1          1
#> 2 MS_Zoning          1     0.0102     0.113                    1          1
#> 3 Lot_Area           0.255 0.0144     0.140                    1          1
#> 4 Street             1     0.0000308  0.00365                  1          1
#> 5 Lot_Frontage       0.165 0.00693    0.146                    0          0
```

## Contributing

Please note that the filtro project is released with a [Contributor Code
of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.

- For questions and discussions about tidymodels packages, modeling, and
  machine learning, please [post on Posit
  Community](https://forum.posit.co/new-topic?category_id=15&tags=tidymodels,question).

- If you think you have encountered a bug, please [submit an
  issue](https://github.com/tidymodels/filtro/issues).

- Either way, learn how to create and share a
  [reprex](https://reprex.tidyverse.org/articles/articles/learn-reprex.html)
  (a minimal, reproducible example), to clearly communicate about your
  code.

- Check out further details on [contributing guidelines for tidymodels
  packages](https://www.tidymodels.org/contribute/) and [how to get
  help](https://www.tidymodels.org/help/).
