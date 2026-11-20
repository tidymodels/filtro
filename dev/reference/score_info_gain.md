# Scoring via entropy-based filters

Three different information theory (entropy) scores can be computed.

## Usage

``` r
score_info_gain

score_gain_ratio

score_sym_uncert
```

## Format

An object of class
[`filtro::class_score_info_gain`](https://filtro.tidymodels.org/dev/reference/class_score.md)
(inherits from
[`filtro::class_score`](https://filtro.tidymodels.org/dev/reference/class_score.md),
`S7_object`) of length 1.

An object of class
[`filtro::class_score_info_gain`](https://filtro.tidymodels.org/dev/reference/class_score.md)
(inherits from
[`filtro::class_score`](https://filtro.tidymodels.org/dev/reference/class_score.md),
`S7_object`) of length 1.

An object of class
[`filtro::class_score_info_gain`](https://filtro.tidymodels.org/dev/reference/class_score.md)
(inherits from
[`filtro::class_score`](https://filtro.tidymodels.org/dev/reference/class_score.md),
`S7_object`) of length 1.

## Value

An S7 object. The primary property of interest is in `results`. This is
a data frame of results that is populated by the
[`fit()`](https://generics.r-lib.org/reference/fit.html) method and has
columns:

- `name`: The name of the score (e.g., `info_gain`).

- `score`: The estimates for each predictor.

- `outcome`: The name of the outcome column.

- `predictor`: The names of the predictor inputs.

These data are accessed using `object@results` (see examples below).

## Details

These objects are used when either:

- The predictors are numeric and the outcome is a factor/category, or

- The predictors are factors and the outcome is numeric.

In either case, an entropy-based filter (via
[`FSelectorRcpp::information_gain()`](https://rdrr.io/pkg/FSelectorRcpp/man/information_gain.html))
is applied with the proper variable roles. Depending on the chosen
method, information gain, gain ratio, or symmetrical uncertainty is
computed. Larger values are associated with more important predictors.

### Estimating the scores

In filtro, the `score_*` objects define a scoring method (e.g., data
input requirements, package dependencies, etc). To compute the scores
for a specific data set, the
[`fit()`](https://generics.r-lib.org/reference/fit.html) method is used.
The main arguments for these functions are:

- `object`:

  A score class object (e.g., `score_info_gain`).

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
[`score_cor_pearson`](https://filtro.tidymodels.org/dev/reference/score_cor_pearson.md),
[`score_imp_rf`](https://filtro.tidymodels.org/dev/reference/score_imp_rf.md),
[`score_roc_auc`](https://filtro.tidymodels.org/dev/reference/score_roc_auc.md),
[`score_xtab_pval_chisq`](https://filtro.tidymodels.org/dev/reference/score_xtab_pval_chisq.md)

## Examples

``` r

library(dplyr)

# Entropy-based filter for classification tasks

cells_subset <- modeldata::cells |>
  dplyr::select(
    class,
    angle_ch_1,
    area_ch_1,
    avg_inten_ch_1,
    avg_inten_ch_2,
    avg_inten_ch_3
  )

# Information gain
cells_info_gain_res <- score_info_gain |>
  fit(class ~ ., data = cells_subset)
cells_info_gain_res@results
#> # A tibble: 5 × 4
#>   name      score outcome predictor     
#>   <chr>     <dbl> <chr>   <chr>         
#> 1 infogain 0      class   angle_ch_1    
#> 2 infogain 0.0144 class   area_ch_1     
#> 3 infogain 0.109  class   avg_inten_ch_1
#> 4 infogain 0.137  class   avg_inten_ch_2
#> 5 infogain 0      class   avg_inten_ch_3

# Gain ratio
cells_gain_ratio_res <- score_gain_ratio |>
  fit(class ~ ., data = cells_subset)
cells_gain_ratio_res@results
#> # A tibble: 5 × 4
#>   name       score outcome predictor     
#>   <chr>      <dbl> <chr>   <chr>         
#> 1 gainratio 0      class   angle_ch_1    
#> 2 gainratio 0.0266 class   area_ch_1     
#> 3 gainratio 0.0828 class   avg_inten_ch_1
#> 4 gainratio 0.106  class   avg_inten_ch_2
#> 5 gainratio 0      class   avg_inten_ch_3

# Symmetrical uncertainty
cells_sym_uncert_res <- score_sym_uncert |>
  fit(class ~ ., data = cells_subset)
cells_sym_uncert_res@results
#> # A tibble: 5 × 4
#>   name       score outcome predictor     
#>   <chr>      <dbl> <chr>   <chr>         
#> 1 symuncert 0      class   angle_ch_1    
#> 2 symuncert 0.0242 class   area_ch_1     
#> 3 symuncert 0.111  class   avg_inten_ch_1
#> 4 symuncert 0.141  class   avg_inten_ch_2
#> 5 symuncert 0      class   avg_inten_ch_3

# ----------------------------------------------------------------------------

# Entropy-based filter for regression tasks

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

regression_task <- score_info_gain
regression_task@mode <- "regression"

ames_info_gain_regression_task_res <-
  regression_task |>
  fit(Sale_Price ~ ., data = ames_subset)
ames_info_gain_regression_task_res@results
#> # A tibble: 5 × 4
#>   name       score outcome    predictor   
#>   <chr>      <dbl> <chr>      <chr>       
#> 1 infogain 0.266   Sale_Price MS_SubClass 
#> 2 infogain 0.113   Sale_Price MS_Zoning   
#> 3 infogain 0.146   Sale_Price Lot_Frontage
#> 4 infogain 0.140   Sale_Price Lot_Area    
#> 5 infogain 0.00365 Sale_Price Street      
```
