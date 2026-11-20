# Show best score, based on number of predictors *(singular)*

Show best score, based on number of predictors *(singular)*

## Arguments

- x:

  A score class object (e.g., `score_cor_pearson`).

- ...:

  Further arguments passed to or from other methods.

- num_terms:

  An integer value specifying the number of predictors to consider.

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
ames_aov_pval_res |> show_best_score_num(num_terms = 2)
#> # A tibble: 2 × 4
#>   name     score outcome    predictor  
#>   <chr>    <dbl> <chr>      <chr>      
#> 1 aov_pval  237. Sale_Price MS_SubClass
#> 2 aov_pval  130. Sale_Price MS_Zoning  
```
