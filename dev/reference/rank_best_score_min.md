# Rank score based on `dplyr::min_rank()`, where tied values receive the same rank and ranks are with gaps *(singular)*

Rank score based on
[`dplyr::min_rank()`](https://dplyr.tidyverse.org/reference/row_number.html),
where tied values receive the same rank and ranks are with gaps
*(singular)*

## Usage

``` r
rank_best_score_min(x, ...)
```

## Arguments

- x:

  A score class object (e.g., `score_cor_pearson`).

- ...:

  Further arguments passed to or from other methods.

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

# Rank score
ames_aov_pval_res |> rank_best_score_min()
#> # A tibble: 5 × 5
#>   name      score outcome    predictor     rank
#>   <chr>     <dbl> <chr>      <chr>        <int>
#> 1 aov_pval 237.   Sale_Price MS_SubClass      1
#> 2 aov_pval 130.   Sale_Price MS_Zoning        2
#> 3 aov_pval  NA    Sale_Price Lot_Frontage    NA
#> 4 aov_pval  NA    Sale_Price Lot_Area        NA
#> 5 aov_pval   5.75 Sale_Price Street           3
```
