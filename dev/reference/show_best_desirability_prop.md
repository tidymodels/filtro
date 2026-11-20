# Show best desirability scores, based on proportion of predictors *(plural)*

Analogous to, and adapted from
[`desirability2::show_best_desirability()`](https://desirability2.tidymodels.org/reference/show_best_desirability.html)
that can simultaneously optimize multiple scores using desirability
functions. See
[`show_best_score_prop()`](https://filtro.tidymodels.org/dev/reference/show_best_score_prop.md)
for *singular* filtering method.

## Usage

``` r
show_best_desirability_prop(x, ..., prop_terms = 1)
```

## Arguments

- x:

  A tibble or data frame returned by
  [`fill_safe_values()`](https://filtro.tidymodels.org/dev/reference/fill_safe_values.md).

- ...:

  One or more desirability selectors to configure the optimization.

- prop_terms:

  A numeric value specifying the proportion of predictors to consider.

## Value

A tibble with `prop_terms` proportion of rows. When showing the results,
the metrics are presented in "wide format" (one column per metric) and
there are new columns for the corresponding desirability values (each
starts with `.d_`).

## Details

Desirability functions might help when selecting the best model based on
more than one performance metric. The user creates a desirability
function to map values of a metric to a `[0, 1]` range where 1.0 is most
desirable and zero is unacceptable. After constructing these for the
metric of interest, the overall desirability is computed using the
geometric mean of the individual desirabilities.

The verbs that can be used in `...` (and their arguments) are:

- [`maximize()`](https://desirability2.tidymodels.org/reference/aliases.html)
  when larger values are better, such as the area under the ROC score.

- [`minimize()`](https://desirability2.tidymodels.org/reference/aliases.html)
  for metrics such as RMSE or the Brier score.

- [`target()`](https://desirability2.tidymodels.org/reference/aliases.html)
  for cases when a specific value of the metric is important.

- [`constrain()`](https://desirability2.tidymodels.org/reference/aliases.html)
  is used when there is a range of values that are equally desirable.

## Examples

``` r

library(desirability2)
library(dplyr)

# Remove outcome
ames_scores_results <- ames_scores_results |>
  dplyr::select(-outcome)
ames_scores_results
#> # A tibble: 5 × 5
#>   predictor    aov_pval cor_pearson    imp_rf infogain
#>   <chr>           <dbl>       <dbl>     <dbl>    <dbl>
#> 1 MS_SubClass    237.         1     0.0148     0.266  
#> 2 MS_Zoning      130.         1     0.00997    0.113  
#> 3 Lot_Frontage   Inf          0.165 0.00668    0.146  
#> 4 Lot_Area       Inf          0.255 0.0137     0.140  
#> 5 Street           5.75       1     0.0000455  0.00365

show_best_desirability_prop(
  ames_scores_results,
  maximize(cor_pearson, low = 0, high = 1)
)
#> # A tibble: 5 × 7
#>   predictor    aov_pval cor_pearson  imp_rf infogain .d_max_cor_pearson
#>   <chr>           <dbl>       <dbl>   <dbl>    <dbl>              <dbl>
#> 1 MS_SubClass    237.         1     1.48e-2  0.266                1    
#> 2 MS_Zoning      130.         1     9.97e-3  0.113                1    
#> 3 Street           5.75       1     4.55e-5  0.00365              1    
#> 4 Lot_Area       Inf          0.255 1.37e-2  0.140                0.255
#> 5 Lot_Frontage   Inf          0.165 6.68e-3  0.146                0.165
#> # ℹ 1 more variable: .d_overall <dbl>

show_best_desirability_prop(
  ames_scores_results,
  maximize(cor_pearson, low = 0, high = 1),
  maximize(imp_rf)
)
#> # A tibble: 5 × 8
#>   predictor    aov_pval cor_pearson  imp_rf infogain .d_max_cor_pearson
#>   <chr>           <dbl>       <dbl>   <dbl>    <dbl>              <dbl>
#> 1 MS_SubClass    237.         1     1.48e-2  0.266                1    
#> 2 MS_Zoning      130.         1     9.97e-3  0.113                1    
#> 3 Lot_Area       Inf          0.255 1.37e-2  0.140                0.255
#> 4 Lot_Frontage   Inf          0.165 6.68e-3  0.146                0.165
#> 5 Street           5.75       1     4.55e-5  0.00365              1    
#> # ℹ 2 more variables: .d_max_imp_rf <dbl>, .d_overall <dbl>

show_best_desirability_prop(
  ames_scores_results,
  maximize(cor_pearson, low = 0, high = 1),
  maximize(imp_rf),
  maximize(infogain)
)
#> # A tibble: 5 × 9
#>   predictor    aov_pval cor_pearson  imp_rf infogain .d_max_cor_pearson
#>   <chr>           <dbl>       <dbl>   <dbl>    <dbl>              <dbl>
#> 1 MS_SubClass    237.         1     1.48e-2  0.266                1    
#> 2 MS_Zoning      130.         1     9.97e-3  0.113                1    
#> 3 Lot_Area       Inf          0.255 1.37e-2  0.140                0.255
#> 4 Lot_Frontage   Inf          0.165 6.68e-3  0.146                0.165
#> 5 Street           5.75       1     4.55e-5  0.00365              1    
#> # ℹ 3 more variables: .d_max_imp_rf <dbl>, .d_max_infogain <dbl>,
#> #   .d_overall <dbl>

show_best_desirability_prop(
  ames_scores_results,
  maximize(cor_pearson, low = 0, high = 1),
  maximize(imp_rf),
  maximize(infogain),
  prop_terms = 0.2
)
#> # A tibble: 1 × 9
#>   predictor   aov_pval cor_pearson imp_rf infogain .d_max_cor_pearson
#>   <chr>          <dbl>       <dbl>  <dbl>    <dbl>              <dbl>
#> 1 MS_SubClass     237.           1 0.0148    0.266                  1
#> # ℹ 3 more variables: .d_max_imp_rf <dbl>, .d_max_infogain <dbl>,
#> #   .d_overall <dbl>

show_best_desirability_prop(
  ames_scores_results,
  target(cor_pearson, low = 0.2, target = 0.255, high = 0.9)
)
#> # A tibble: 5 × 7
#>   predictor aov_pval cor_pearson  imp_rf infogain .d_target_cor_pearson
#>   <chr>        <dbl>       <dbl>   <dbl>    <dbl>                 <dbl>
#> 1 Lot_Area    Inf          0.255 1.37e-2  0.140                   1.000
#> 2 MS_SubCl…   237.         1     1.48e-2  0.266                   0    
#> 3 MS_Zoning   130.         1     9.97e-3  0.113                   0    
#> 4 Lot_Fron…   Inf          0.165 6.68e-3  0.146                   0    
#> 5 Street        5.75       1     4.55e-5  0.00365                 0    
#> # ℹ 1 more variable: .d_overall <dbl>

show_best_desirability_prop(
  ames_scores_results,
  constrain(cor_pearson, low = 0.2, high = 1)
)
#> # A tibble: 5 × 7
#>   predictor    aov_pval cor_pearson  imp_rf infogain .d_box_cor_pearson
#>   <chr>           <dbl>       <dbl>   <dbl>    <dbl>              <dbl>
#> 1 MS_SubClass    237.         1     1.48e-2  0.266                    1
#> 2 MS_Zoning      130.         1     9.97e-3  0.113                    1
#> 3 Lot_Area       Inf          0.255 1.37e-2  0.140                    1
#> 4 Street           5.75       1     4.55e-5  0.00365                  1
#> 5 Lot_Frontage   Inf          0.165 6.68e-3  0.146                    0
#> # ℹ 1 more variable: .d_overall <dbl>
```
