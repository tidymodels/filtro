# Show best score, based on proportion of predictors *(singular)*

Show best score, based on proportion of predictors *(singular)*

## Arguments

- x:

  A score class object (e.g., `score_cor_pearson`).

- ...:

  Further arguments passed to or from other methods.

- prop_terms:

  A numeric value specifying the proportion of predictors to consider.

## Value

A tibble of score results.

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

# Show best score
ames_aov_pval_res |> show_best_score_prop(prop_terms = 0.2)
#> # A tibble: 1 × 4
#>   name     score outcome    predictor  
#>   <chr>    <dbl> <chr>      <chr>      
#> 1 aov_pval  237. Sale_Price MS_SubClass
```
