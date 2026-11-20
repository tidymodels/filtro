# Bind score class object, including their associated metadata and scores

Binds multiple score class objects (e.g., `score_*`), including their
associated metadata and scores. See
[`fill_safe_values()`](https://filtro.tidymodels.org/dev/reference/fill_safe_values.md)
for binding with safe-value handling.

## Arguments

- x:

  A list.

- ...:

  Further arguments passed to or from other methods.

## Value

A tibble of scores results.

## Examples

``` r

library(dplyr)

ames_subset <- modeldata::ames |>
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

# Forest importance
set.seed(42)
ames_imp_rf_reg_res <-
  score_imp_rf |>
  fit(Sale_Price ~ ., data = ames_subset)
ames_imp_rf_reg_res@results
#> # A tibble: 5 × 4
#>   name       score outcome    predictor   
#>   <chr>      <dbl> <chr>      <chr>       
#> 1 imp_rf 0.0147    Sale_Price MS_SubClass 
#> 2 imp_rf 0.0105    Sale_Price MS_Zoning   
#> 3 imp_rf 0.00691   Sale_Price Lot_Frontage
#> 4 imp_rf 0.0142    Sale_Price Lot_Area    
#> 5 imp_rf 0.0000347 Sale_Price Street      

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

# Create a list
class_score_list <- list(
  ames_aov_pval_res,
  ames_cor_pearson_res,
  ames_imp_rf_reg_res,
  ames_info_gain_reg_res
)

# Bind scores
class_score_list |> bind_scores()
#> # A tibble: 5 × 6
#>   outcome    predictor    aov_pval cor_pearson    imp_rf infogain
#>   <chr>      <chr>           <dbl>       <dbl>     <dbl>    <dbl>
#> 1 Sale_Price MS_SubClass    237.        NA     0.0147     0.266  
#> 2 Sale_Price MS_Zoning      130.        NA     0.0105     0.113  
#> 3 Sale_Price Lot_Frontage    NA          0.165 0.00691    0.146  
#> 4 Sale_Price Lot_Area        NA          0.255 0.0142     0.140  
#> 5 Sale_Price Street           5.75      NA     0.0000347  0.00365
```
