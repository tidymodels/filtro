# Introduction to filtro

> ⚠️ **work-in-progress**

This document demonstrates some basic uses of filtro. We’ll need to load
a few packages:

``` r

library(filtro)
library(desirability2)
library(dplyr)
library(modeldata)
```

## A scoring example

The {modeldata} package contains a data set used to predict housing sale
price. It has 73 predictor columns and a numeric variable `Sale_Price`
(the outcome). Since the outcome are right-skewed, we apply a log (base
10) transformation.

``` r

ames <- modeldata::ames
ames <- ames |>
  dplyr::mutate(Sale_Price = log10(Sale_Price))

# ames |> str() # uncomment to see the structure of the data
```

To apply the ANOVA F-test filter, we first create a score class object
to define the scoring method, and then use the
[`fit()`](https://generics.r-lib.org/reference/fit.html) method with the
standard formula to compute the scores.

``` r

ames_aov_pval_res <-
  score_aov_pval |>
  fit(Sale_Price ~ ., data = ames)
```

The data frame of results can be accessed via `object@results`.

``` r

ames_aov_pval_res@results
#> # A tibble: 73 × 4
#>    name      score outcome    predictor   
#>    <chr>     <dbl> <chr>      <chr>       
#>  1 aov_pval 237.   Sale_Price MS_SubClass 
#>  2 aov_pval 130.   Sale_Price MS_Zoning   
#>  3 aov_pval  NA    Sale_Price Lot_Frontage
#>  4 aov_pval  NA    Sale_Price Lot_Area    
#>  5 aov_pval   5.75 Sale_Price Street      
#>  6 aov_pval  19.2  Sale_Price Alley       
#>  7 aov_pval  71.3  Sale_Price Lot_Shape   
#>  8 aov_pval  21.4  Sale_Price Land_Contour
#>  9 aov_pval   1.38 Sale_Price Utilities   
#> 10 aov_pval  12.0  Sale_Price Lot_Config  
#> # ℹ 63 more rows
```

A couple of notes here:

Since our focus is on feature relevance (rather than hypothesis
testing), the ANOVA F-test filter handles both cases when:

- The predictors are numeric and the outcome is categorical, or

- The predictors are categorical and the outcome is numeric.

Because the outcome is numeric, any predictor that is not a factor will
result in an `NA`. In case where `NA` is produced, a safe value can be
used to retain the predictor, and can be accessed via
`object@fallback_value`.

By default, this filter computes `-log10(p_value)`, so that larger
values indicate more important predictors. If users prefer raw p-values,
a helper function
[`dont_log_pvalues()`](https://filtro.tidymodels.org/dev/reference/dont_log_pvalues.md)
is available.

For this specific filter, i.e., `score_aov_*`, case weights are
supported. For other filters, you can check the property
`object@case_weights` to see if they can use case weights.

## Filtering and ranking

There are two main ways to rank and select a top proportion or number of
features.

To filter or rank a single score, we can use built-in methods:

- `show_best_score_*()`

- `rank_best_score_*()`

For multi-parameter optimization, we can use API calls adapted from
{desirability}:

- `show_best_desirability_*()`

## A filtering exmple for score *singular*

The
[`show_best_score_prop()`](https://filtro.tidymodels.org/dev/reference/show_best_score_prop.md)
function returns the best score for a single metric. The `prop_terms`
argument lets us control the proportion of predictors to keep.

``` r

# Show best score, based on proportion of predictors
ames_aov_pval_res |> show_best_score_prop(prop_terms = 0.2)
#> # A tibble: 14 × 4
#>    name     score outcome    predictor     
#>    <chr>    <dbl> <chr>      <chr>         
#>  1 aov_pval Inf   Sale_Price Neighborhood  
#>  2 aov_pval 288.  Sale_Price Garage_Finish 
#>  3 aov_pval 243.  Sale_Price Garage_Type   
#>  4 aov_pval 242.  Sale_Price Foundation    
#>  5 aov_pval 237.  Sale_Price MS_SubClass   
#>  6 aov_pval 183.  Sale_Price Heating_QC    
#>  7 aov_pval 173.  Sale_Price BsmtFin_Type_1
#>  8 aov_pval 132.  Sale_Price Mas_Vnr_Type  
#>  9 aov_pval 130.  Sale_Price Overall_Cond  
#> 10 aov_pval 130.  Sale_Price MS_Zoning     
#> 11 aov_pval 127.  Sale_Price Exterior_1st  
#> 12 aov_pval 116.  Sale_Price Exterior_2nd  
#> 13 aov_pval 116.  Sale_Price Bsmt_Exposure 
#> 14 aov_pval 100.0 Sale_Price Garage_Cond
```

## A filtering example for scores *plural*

To handle multiple scores, we first create multiple score class objects,
and then use the
[`fit()`](https://generics.r-lib.org/reference/fit.html) method with the
standard formula to compute the scores.

``` r

# ANOVA raw p-value 
natrual_units <- score_aov_pval |> dont_log_pvalues()
ames_aov_pval_natrual_res <-
  natrual_units |>
  fit(Sale_Price ~ ., data = ames)

# Pearson correlation
ames_cor_pearson_res <-
  score_cor_pearson |>
  fit(Sale_Price ~ ., data = ames)

# Forest importance
ames_imp_rf_reg_res <-
  score_imp_rf |>
  fit(Sale_Price ~ ., data = ames, seed = 42)

# Information gain
ames_info_gain_reg_res <-
  score_info_gain |>
  fit(Sale_Price ~ ., data = ames)
```

Next, we create a list to collect these score class objects, including
their associated metadata and scores.

``` r

# Create a list
class_score_list <- list(
  ames_aov_pval_natrual_res, 
  ames_cor_pearson_res,
  ames_imp_rf_reg_res,
  ames_info_gain_reg_res
)
```

Then, we fill the safe value specific to each method, and then remove
the `outcome` column.

``` r

# Fill safe values
ames_scores_results <- class_score_list |>
  fill_safe_values() |>
  # Remove outcome
  dplyr::select(-outcome)
ames_scores_results
#> # A tibble: 73 × 5
#>    predictor     aov_pval cor_pearson     imp_rf infogain
#>    <chr>            <dbl>       <dbl>      <dbl>    <dbl>
#>  1 MS_SubClass  1.68e-237       1     0.000449    0.266  
#>  2 MS_Zoning    2.75e-130       1     0.000386    0.113  
#>  3 Lot_Frontage 1.11e- 16       0.165 0.000194    0.146  
#>  4 Lot_Area     1.11e- 16       0.255 0.000736    0.140  
#>  5 Street       1.77e-  6       1     0.00000263  0.00365
#>  6 Alley        6.06e- 20       1     0.00000782  0.0254 
#>  7 Lot_Shape    5.17e- 72       1     0.0000880   0.0675 
#>  8 Land_Contour 3.79e- 22       1     0.0000480   0.0212 
#>  9 Utilities    4.16e-  2       1     0           0.00165
#> 10 Lot_Config   1.04e- 12       1     0.0000138   0.0133 
#> # ℹ 63 more rows
```

Analogous to
[`show_best_desirability()`](https://desirability2.tidymodels.org/reference/show_best_desirability.html),
the
[`show_best_desirability_prop()`](https://filtro.tidymodels.org/dev/reference/show_best_desirability_prop.md)
function allows joint optimization of multiple metrics using
desirability functions.

A desirability function maps values of a metric to a $`[0, 1]`$ range
where $`1`$ is most desirable and $`0`$ is unacceptable. When the verb
[`maximize()`](https://desirability2.tidymodels.org/reference/aliases.html)
is used, it means larger values are better. This is the case for Pearson
correlation, forest importance, and information gain.

For examples:

``` r

# Optimize correlation alone
ames_scores_results |>
  show_best_desirability_prop(
    maximize(cor_pearson, low = 0, high = 1)
  ) |> 
  # Show predictor and desirability only
  dplyr::select(predictor, starts_with(".d_"))
#> # A tibble: 73 × 3
#>    predictor    .d_max_cor_pearson .d_overall
#>    <chr>                     <dbl>      <dbl>
#>  1 MS_SubClass                   1          1
#>  2 MS_Zoning                     1          1
#>  3 Street                        1          1
#>  4 Alley                         1          1
#>  5 Lot_Shape                     1          1
#>  6 Land_Contour                  1          1
#>  7 Utilities                     1          1
#>  8 Lot_Config                    1          1
#>  9 Land_Slope                    1          1
#> 10 Neighborhood                  1          1
#> # ℹ 63 more rows

# Optimize correlation and forest importance
ames_scores_results |>
  show_best_desirability_prop(
    maximize(cor_pearson, low = 0, high = 1),
    maximize(imp_rf)
  ) |> 
  dplyr::select(predictor, starts_with(".d_"))
#> # A tibble: 73 × 4
#>    predictor      .d_max_cor_pearson .d_max_imp_rf .d_overall
#>    <chr>                       <dbl>         <dbl>      <dbl>
#>  1 Gr_Liv_Area                 0.696         1          0.834
#>  2 Year_Built                  0.615         0.877      0.735
#>  3 Total_Bsmt_SF               0.626         0.594      0.610
#>  4 Year_Remod_Add              0.586         0.549      0.567
#>  5 Garage_Type                 1             0.308      0.555
#>  6 First_Flr_SF                0.603         0.474      0.534
#>  7 Garage_Cars                 0.675         0.417      0.530
#>  8 Garage_Area                 0.651         0.432      0.530
#>  9 Full_Bath                   0.577         0.308      0.421
#> 10 Foundation                  1             0.151      0.388
#> # ℹ 63 more rows

# Optimize correlation, forest importance and information gain
ames_scores_results |>
  show_best_desirability_prop(
    maximize(cor_pearson, low = 0, high = 1),
    maximize(imp_rf),
    maximize(infogain)
  ) |> 
  dplyr::select(predictor, starts_with(".d_"))
#> # A tibble: 73 × 5
#>    predictor      .d_max_cor_pearson .d_max_imp_rf .d_max_infogain .d_overall
#>    <chr>                       <dbl>         <dbl>           <dbl>      <dbl>
#>  1 Gr_Liv_Area                 0.696         1               0.832      0.833
#>  2 Year_Built                  0.615         0.877           0.709      0.726
#>  3 Total_Bsmt_SF               0.626         0.594           0.625      0.615
#>  4 Garage_Cars                 0.675         0.417           0.708      0.584
#>  5 Garage_Area                 0.651         0.432           0.684      0.577
#>  6 Year_Remod_Add              0.586         0.549           0.514      0.549
#>  7 First_Flr_SF                0.603         0.474           0.551      0.540
#>  8 Garage_Type                 1             0.308           0.453      0.519
#>  9 Neighborhood                1             0.127           1          0.503
#> 10 Full_Bath                   0.577         0.308           0.527      0.454
#> # ℹ 63 more rows
```

In
[`show_best_desirability_prop()`](https://filtro.tidymodels.org/dev/reference/show_best_desirability_prop.md),
there is a argument called `prop_terms` that lets us control the
proportion of predictors to keep.

``` r

# Same as above, but retain only a proportion of predictors
ames_scores_results |>
  show_best_desirability_prop(
    maximize(cor_pearson, low = 0, high = 1),
    maximize(imp_rf),
    maximize(infogain),
    prop_terms = 0.2
  ) |>
  dplyr::select(predictor, starts_with(".d_"))
#> # A tibble: 14 × 5
#>    predictor      .d_max_cor_pearson .d_max_imp_rf .d_max_infogain .d_overall
#>    <chr>                       <dbl>         <dbl>           <dbl>      <dbl>
#>  1 Gr_Liv_Area                 0.696        1                0.832      0.833
#>  2 Year_Built                  0.615        0.877            0.709      0.726
#>  3 Total_Bsmt_SF               0.626        0.594            0.625      0.615
#>  4 Garage_Cars                 0.675        0.417            0.708      0.584
#>  5 Garage_Area                 0.651        0.432            0.684      0.577
#>  6 Year_Remod_Add              0.586        0.549            0.514      0.549
#>  7 First_Flr_SF                0.603        0.474            0.551      0.540
#>  8 Garage_Type                 1            0.308            0.453      0.519
#>  9 Neighborhood                1            0.127            1          0.503
#> 10 Full_Bath                   0.577        0.308            0.527      0.454
#> 11 Foundation                  1            0.151            0.454      0.409
#> 12 MS_SubClass                 1            0.109            0.576      0.398
#> 13 Garage_Finish               1            0.0837           0.501      0.347
#> 14 Fireplaces                  0.489        0.241            0.331      0.339
```

Besides
[`maximize()`](https://desirability2.tidymodels.org/reference/aliases.html),
additional verbs that are available are:
[`minimize()`](https://desirability2.tidymodels.org/reference/aliases.html),
[`target()`](https://desirability2.tidymodels.org/reference/aliases.html),
and
[`constrain()`](https://desirability2.tidymodels.org/reference/aliases.html).
They are used in different situations:

- [`maximize()`](https://desirability2.tidymodels.org/reference/aliases.html)
  when larger values are better.

- [`minimize()`](https://desirability2.tidymodels.org/reference/aliases.html)
  when smaller values are better.

- [`target()`](https://desirability2.tidymodels.org/reference/aliases.html)
  when a specific value of the metric is important.

- [`constrain()`](https://desirability2.tidymodels.org/reference/aliases.html)
  when a range of values is equally desirable.

For examples:

``` r

ames_scores_results |>
  show_best_desirability_prop(
    minimize(aov_pval, low = 0, high = 1)
  ) |> 
  dplyr::select(predictor, starts_with(".d_"))
#> # A tibble: 73 × 3
#>    predictor    .d_min_aov_pval .d_overall
#>    <chr>                  <dbl>      <dbl>
#>  1 MS_SubClass                1          1
#>  2 MS_Zoning                  1          1
#>  3 Alley                      1          1
#>  4 Lot_Shape                  1          1
#>  5 Land_Contour               1          1
#>  6 Neighborhood               1          1
#>  7 Condition_1                1          1
#>  8 Bldg_Type                  1          1
#>  9 House_Style                1          1
#> 10 Overall_Cond               1          1
#> # ℹ 63 more rows

ames_scores_results |>
  show_best_desirability_prop(
    target(cor_pearson, low = 0.2, target = 0.255, high = 0.9)
  ) |> 
  dplyr::select(predictor, starts_with(".d_"))
#> # A tibble: 73 × 3
#>    predictor      .d_target_cor_pearson .d_overall
#>    <chr>                          <dbl>      <dbl>
#>  1 Lot_Area                       1.000      1.000
#>  2 Second_Flr_SF                  0.969      0.969
#>  3 Bsmt_Full_Bath                 0.969      0.969
#>  4 Latitude                       0.952      0.952
#>  5 Half_Bath                      0.921      0.921
#>  6 Open_Porch_SF                  0.899      0.899
#>  7 Wood_Deck_SF                   0.879      0.879
#>  8 Mas_Vnr_Area                   0.709      0.709
#>  9 Fireplaces                     0.637      0.637
#> 10 TotRms_AbvGrd                  0.632      0.632
#> # ℹ 63 more rows

ames_scores_results |>
  show_best_desirability_prop(
    constrain(cor_pearson, low = 0.2, high = 1)
  ) |> 
  dplyr::select(predictor, starts_with(".d_"))
#> # A tibble: 73 × 3
#>    predictor    .d_box_cor_pearson .d_overall
#>    <chr>                     <dbl>      <dbl>
#>  1 MS_SubClass                   1          1
#>  2 MS_Zoning                     1          1
#>  3 Lot_Area                      1          1
#>  4 Street                        1          1
#>  5 Alley                         1          1
#>  6 Lot_Shape                     1          1
#>  7 Land_Contour                  1          1
#>  8 Utilities                     1          1
#>  9 Lot_Config                    1          1
#> 10 Land_Slope                    1          1
#> # ℹ 63 more rows
```

## Available score objects and filter methods

The list of score class objects included:

    #>  [1] "score_aov_fstat"          "score_aov_pval"
    #>  [3] "score_cor_pearson"        "score_cor_spearman"
    #>  [5] "score_gain_ratio"         "score_imp_rf"
    #>  [7] "score_imp_rf_conditional" "score_imp_rf_oblique"
    #>  [9] "score_info_gain"          "score_roc_auc"
    #> [11] "score_sym_uncert"         "score_xtab_pval_chisq"
    #> [13] "score_xtab_pval_fisher"

The list of filter methods for score *singular*:

    #> [1] "show_best_score_cutoff" "show_best_score_dual"   "show_best_score_num"
    #> [4] "show_best_score_prop"

The list of filter methods for scores *plural*:

    #> [1] "show_best_desirability_num"  "show_best_desirability_prop"
