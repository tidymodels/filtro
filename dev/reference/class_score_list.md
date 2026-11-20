# S7 subclass of base R's `list` for method dispatch

`class_score_list` is an S7 subclass of S3 base R's `list`, used for
method dispatch in
[`bind_scores()`](https://filtro.tidymodels.org/dev/reference/bind_scores.md)
and
[`fill_safe_values()`](https://filtro.tidymodels.org/dev/reference/fill_safe_values.md).

## Usage

``` r
class_score_list
```

## Format

An object of class `S7_S3_class` of length 3.

## Value

A list of S7 objects.

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

# Create a list
class_score_list <- list(
  ames_aov_pval_res,
  ames_cor_pearson_res
)
```
