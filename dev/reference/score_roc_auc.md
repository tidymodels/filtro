# Scoring via area under the Receiver Operating Characteristic curve (ROC AUC)

The area under the ROC curves can be used to measure predictor
importance.

## Usage

``` r
score_roc_auc
```

## Format

An object of class
[`filtro::class_score_roc_auc`](https://filtro.tidymodels.org/dev/reference/class_score.md)
(inherits from
[`filtro::class_score`](https://filtro.tidymodels.org/dev/reference/class_score.md),
`S7_object`) of length 1.

## Value

An S7 object. The primary property of interest is in `results`. This is
a data frame of results that is populated by the
[`fit()`](https://generics.r-lib.org/reference/fit.html) method and has
columns:

- `name`: The name of the score (e.g., `roc_auc`).

- `score`: The estimates for each predictor.

- `outcome`: The name of the outcome column.

- `predictor`: The names of the predictor inputs.

These data are accessed using `object@results` (see examples below).

## Details

This objects are used when either:

- The predictors are numeric and the outcome is a factor/category, or

- The predictors are factors and the outcome is numeric.

In either case, a ROC curve (via
[`pROC::roc()`](https://rdrr.io/pkg/pROC/man/roc.html) or
[`pROC::multiclass.roc()`](https://rdrr.io/pkg/pROC/man/multiclass.html))
is created with the proper variable roles, and the area under the ROC
curve is computed (via
[`pROC::auc()`](https://rdrr.io/pkg/pROC/man/auc.html)). Values higher
than 0.5 (i.e., `max(roc_auc, 1 - roc_auc)` \> 0.5) are associated with
more important predictors.

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
  are no case weights. NOTE case weights cannot be used when a
  multiclass ROC is computed.

Missing values are removed for each predictor/outcome combination being
scored.

In cases where the underlying computations fail, the scoring proceeds
silently, and a missing value is given for the score.

## See also

Other class score metrics:
[`score_aov_pval`](https://filtro.tidymodels.org/dev/reference/score_aov_pval.md),
[`score_cor_pearson`](https://filtro.tidymodels.org/dev/reference/score_cor_pearson.md),
[`score_imp_rf`](https://filtro.tidymodels.org/dev/reference/score_imp_rf.md),
[`score_info_gain`](https://filtro.tidymodels.org/dev/reference/score_info_gain.md),
[`score_xtab_pval_chisq`](https://filtro.tidymodels.org/dev/reference/score_xtab_pval_chisq.md)

## Examples

``` r
library(dplyr)

# ROC AUC where the numeric predictors are the predictors and
# `class` is the class outcome/response

cells_subset <- modeldata::cells |>
  dplyr::select(
    class,
    angle_ch_1,
    area_ch_1,
    avg_inten_ch_1,
    avg_inten_ch_2,
    avg_inten_ch_3
  )

cells_roc_auc_res <- score_roc_auc |>
  fit(class ~ ., data = cells_subset)
cells_roc_auc_res@results
#> # A tibble: 5 × 4
#>   name    score outcome predictor     
#>   <chr>   <dbl> <chr>   <chr>         
#> 1 roc_auc 0.502 class   angle_ch_1    
#> 2 roc_auc 0.591 class   area_ch_1     
#> 3 roc_auc 0.760 class   avg_inten_ch_1
#> 4 roc_auc 0.777 class   avg_inten_ch_2
#> 5 roc_auc 0.513 class   avg_inten_ch_3

# ----------------------------------------------------------------------------

# ROC AUC where `Sale_Price` is the numeric predictor and the class predictors
# are the outcomes/responses

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

ames_roc_auc_res <- score_roc_auc |>
  fit(Sale_Price ~ ., data = ames_subset)
ames_roc_auc_res@results
#> # A tibble: 5 × 4
#>   name     score outcome    predictor   
#>   <chr>    <dbl> <chr>      <chr>       
#> 1 roc_auc  0.742 Sale_Price MS_SubClass 
#> 2 roc_auc  0.853 Sale_Price MS_Zoning   
#> 3 roc_auc NA     Sale_Price Lot_Frontage
#> 4 roc_auc NA     Sale_Price Lot_Area    
#> 5 roc_auc  0.807 Sale_Price Street      
# TODO Add multiclass example
```
