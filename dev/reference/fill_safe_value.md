# Fill safe value *(singular)*

Fills in safe value for missing score, with an option to apply
transformation. This is a *singular* scoring method. See
[`fill_safe_values()`](https://filtro.tidymodels.org/dev/reference/fill_safe_values.md)
for *plural* scoring method.

## Arguments

- x:

  A score class object (e.g., `score_cor_pearson`).

- return_results:

  A logical value indicating whether to return results.

## Value

A tibble of score results.

## Details

If `transform = TRUE`, by default, all score objects use the identity
transformation, except the correlation score object, which uses the
absolute transformation.

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

# Fill safe value
ames_aov_pval_res |> fill_safe_value(return_results = TRUE)
#> # A tibble: 5 × 4
#>   name      score outcome    predictor   
#>   <chr>     <dbl> <chr>      <chr>       
#> 1 aov_pval 237.   Sale_Price MS_SubClass 
#> 2 aov_pval 130.   Sale_Price MS_Zoning   
#> 3 aov_pval Inf    Sale_Price Lot_Frontage
#> 4 aov_pval Inf    Sale_Price Lot_Area    
#> 5 aov_pval   5.75 Sale_Price Street      

# Fill safe value, option to transform
ames_aov_pval_res |> fill_safe_value(return_results = TRUE, transform = TRUE)
#> # A tibble: 5 × 4
#>   name      score outcome    predictor   
#>   <chr>     <dbl> <chr>      <chr>       
#> 1 aov_pval 237.   Sale_Price MS_SubClass 
#> 2 aov_pval 130.   Sale_Price MS_Zoning   
#> 3 aov_pval Inf    Sale_Price Lot_Frontage
#> 4 aov_pval Inf    Sale_Price Lot_Area    
#> 5 aov_pval   5.75 Sale_Price Street      
```
