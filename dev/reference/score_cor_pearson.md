# Scoring via correlation coefficient

These two objects can be used to compute importance scores based on
correlation coefficient.

## Usage

``` r
score_cor_pearson

score_cor_spearman
```

## Format

An object of class
[`filtro::class_score_cor`](https://filtro.tidymodels.org/dev/reference/class_score.md)
(inherits from
[`filtro::class_score`](https://filtro.tidymodels.org/dev/reference/class_score.md),
`S7_object`) of length 1.

An object of class
[`filtro::class_score_cor`](https://filtro.tidymodels.org/dev/reference/class_score.md)
(inherits from
[`filtro::class_score`](https://filtro.tidymodels.org/dev/reference/class_score.md),
`S7_object`) of length 1.

## Value

An S7 object. The primary property of interest is in `results`. This is
a data frame of results that is populated by the
[`fit()`](https://generics.r-lib.org/reference/fit.html) method and has
columns:

- `name`: The name of the score (e.g., `score_cor_pearson` or
  `score_cor_spearman`).

- `score`: The estimates for each predictor.

- `outcome`: The name of the outcome column.

- `predictor`: The names of the predictor inputs.

These data are accessed using `object@results` (see examples below).

## Details

These objects are used when:

- The predictors are numeric and the outcome is numeric.

In this case, a correlation coefficient (via
[`stats::cov.wt()`](https://rdrr.io/r/stats/cov.wt.html)) is computed
with the proper variable roles. Values closer to 1 or -1 (i.e.,
`abs(cor_pearson)` closer to 1) are associated with more important
predictors.

### Estimating the scores

In filtro, the `score_*` objects define a scoring method (e.g., data
input requirements, package dependencies, etc). To compute the scores
for a specific data set, the
[`fit()`](https://generics.r-lib.org/reference/fit.html) method is used.
The main arguments for these functions are:

- `object`:

  A score class object (e.g., `score_cor_pearson`).

- `formula`:

  A standard R formula with a single outcome on the right-hand side and
  one or more predictors (or `.`) on the left-hand side. The data are
  processed via
  [`stats::model.frame()`](https://rdrr.io/r/stats/model.frame.html)

- `data`:

  A data frame containing the relevant columns defined by the formula.

- `...`:

  Further arguments passed to or from other methods.

- `case_weights`:

  A quantitative vector of case weights that is the same length as the
  number of rows in `data`. The default of `NULL` indicates that there
  are no case weights.

Missing values are removed for each predictor/outcome combination being
scored.

In cases where the underlying computations fail, the scoring proceeds
silently, and a missing value is given for the score.

## See also

Other class score metrics:
[`score_aov_pval`](https://filtro.tidymodels.org/dev/reference/score_aov_pval.md),
[`score_imp_rf`](https://filtro.tidymodels.org/dev/reference/score_imp_rf.md),
[`score_info_gain`](https://filtro.tidymodels.org/dev/reference/score_info_gain.md),
[`score_roc_auc`](https://filtro.tidymodels.org/dev/reference/score_roc_auc.md),
[`score_xtab_pval_chisq`](https://filtro.tidymodels.org/dev/reference/score_xtab_pval_chisq.md)

## Examples

``` r
library(dplyr)

ames <- modeldata::ames

# Pearson correlation
ames_cor_pearson_res <-
  score_cor_pearson |>
  fit(Sale_Price ~ ., data = ames)
ames_cor_pearson_res@results
#> # A tibble: 73 × 4
#>    name         score outcome    predictor   
#>    <chr>        <dbl> <chr>      <chr>       
#>  1 cor_pearson NA     Sale_Price MS_SubClass 
#>  2 cor_pearson NA     Sale_Price MS_Zoning   
#>  3 cor_pearson  0.202 Sale_Price Lot_Frontage
#>  4 cor_pearson  0.267 Sale_Price Lot_Area    
#>  5 cor_pearson NA     Sale_Price Street      
#>  6 cor_pearson NA     Sale_Price Alley       
#>  7 cor_pearson NA     Sale_Price Lot_Shape   
#>  8 cor_pearson NA     Sale_Price Land_Contour
#>  9 cor_pearson NA     Sale_Price Utilities   
#> 10 cor_pearson NA     Sale_Price Lot_Config  
#> # ℹ 63 more rows

# Spearman correlation
ames_cor_spearman_res <-
  score_cor_spearman |>
  fit(Sale_Price ~ ., data = ames)
ames_cor_spearman_res@results
#> # A tibble: 73 × 4
#>    name          score outcome    predictor   
#>    <chr>         <dbl> <chr>      <chr>       
#>  1 cor_spearman NA     Sale_Price MS_SubClass 
#>  2 cor_spearman NA     Sale_Price MS_Zoning   
#>  3 cor_spearman  0.228 Sale_Price Lot_Frontage
#>  4 cor_spearman  0.429 Sale_Price Lot_Area    
#>  5 cor_spearman NA     Sale_Price Street      
#>  6 cor_spearman NA     Sale_Price Alley       
#>  7 cor_spearman NA     Sale_Price Lot_Shape   
#>  8 cor_spearman NA     Sale_Price Land_Contour
#>  9 cor_spearman NA     Sale_Price Utilities   
#> 10 cor_spearman NA     Sale_Price Lot_Config  
#> # ℹ 63 more rows
```
