# Scoring via random forests

Three different random forest models can be used to measure predictor
importance.

## Usage

``` r
score_imp_rf

score_imp_rf_conditional

score_imp_rf_oblique
```

## Format

An object of class
[`filtro::class_score_imp_rf`](https://filtro.tidymodels.org/dev/reference/class_score.md)
(inherits from
[`filtro::class_score`](https://filtro.tidymodels.org/dev/reference/class_score.md),
`S7_object`) of length 1.

An object of class
[`filtro::class_score_imp_rf`](https://filtro.tidymodels.org/dev/reference/class_score.md)
(inherits from
[`filtro::class_score`](https://filtro.tidymodels.org/dev/reference/class_score.md),
`S7_object`) of length 1.

An object of class
[`filtro::class_score_imp_rf`](https://filtro.tidymodels.org/dev/reference/class_score.md)
(inherits from
[`filtro::class_score`](https://filtro.tidymodels.org/dev/reference/class_score.md),
`S7_object`) of length 1.

## Value

An S7 object. The primary property of interest is in `results`. This is
a data frame of results that is populated by the
[`fit()`](https://generics.r-lib.org/reference/fit.html) method and has
columns:

- `name`: The name of the score (e.g., `imp_rf`).

- `score`: The estimates for each predictor.

- `outcome`: The name of the outcome column.

- `predictor`: The names of the predictor inputs.

These data are accessed using `object@results` (see examples below).

## Details

These objects are used when either:

- The predictors are numeric and the outcome is a factor/category, or

- The predictors are factors and the outcome is numeric.

In either case, a random forest, conditional random forest, or oblique
random forest (via
[`ranger::ranger()`](http://imbs-hl.github.io/ranger/reference/ranger.md),
[`partykit::cforest()`](https://rdrr.io/pkg/partykit/man/cforest.html),
or
[`aorsf::orsf()`](https://docs.ropensci.org/aorsf/reference/orsf.html))
is created with the proper variable roles, and the feature importance
scores are computed. Larger values are associated with more important
predictors.

When a predictor's importance score is 0,
[`partykit::cforest()`](https://rdrr.io/pkg/partykit/man/cforest.html)
may omit its name from the results. In cases like these, a score of 0 is
assigned to the missing predictors.

### Estimating the scores

In filtro, the `score_*` objects define a scoring method (e.g., data
input requirements, package dependencies, etc). To compute the scores
for a specific data set, the
[`fit()`](https://generics.r-lib.org/reference/fit.html) method is used.
The main arguments for these functions are:

- `object`:

  A score class object (e.g., `score_imp_rf`).

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

Missing values are removed by case-wise deletion.

In cases where the underlying computations fail, the scoring proceeds
silently, and a missing value is given for the score.

## See also

Other class score metrics:
[`score_aov_pval`](https://filtro.tidymodels.org/dev/reference/score_aov_pval.md),
[`score_cor_pearson`](https://filtro.tidymodels.org/dev/reference/score_cor_pearson.md),
[`score_info_gain`](https://filtro.tidymodels.org/dev/reference/score_info_gain.md),
[`score_roc_auc`](https://filtro.tidymodels.org/dev/reference/score_roc_auc.md),
[`score_xtab_pval_chisq`](https://filtro.tidymodels.org/dev/reference/score_xtab_pval_chisq.md)

## Examples

``` r

library(dplyr)

# Random forests for classification task

cells_subset <- modeldata::cells |>
  # Use a small example for efficiency
  dplyr::select(
    class,
    angle_ch_1,
    area_ch_1,
    avg_inten_ch_1,
    avg_inten_ch_2,
    avg_inten_ch_3
  ) |>
  slice(1:50)

# Random forest
set.seed(42)
cells_imp_rf_res <- score_imp_rf |>
  fit(class ~ ., data = cells_subset)
cells_imp_rf_res@results
#> # A tibble: 5 × 4
#>   name       score outcome predictor     
#>   <chr>      <dbl> <chr>   <chr>         
#> 1 imp_rf -0.00283  class   angle_ch_1    
#> 2 imp_rf -0.00472  class   area_ch_1     
#> 3 imp_rf  0.0419   class   avg_inten_ch_1
#> 4 imp_rf  0.0604   class   avg_inten_ch_2
#> 5 imp_rf  0.000662 class   avg_inten_ch_3

# Conditional random forest
cells_imp_rf_conditional_res <- score_imp_rf_conditional |>
  fit(class ~ ., data = cells_subset, trees = 10)
cells_imp_rf_conditional_res@results
#> # A tibble: 4 × 4
#>   name                  score outcome predictor     
#>   <chr>                 <dbl> <chr>   <chr>         
#> 1 imp_rf_conditional -0.00889 class   angle_ch_1    
#> 2 imp_rf_conditional  0.0377  class   area_ch_1     
#> 3 imp_rf_conditional  0.199   class   avg_inten_ch_1
#> 4 imp_rf_conditional  0.616   class   avg_inten_ch_2

# Oblique random forest
cells_imp_rf_oblique_res <- score_imp_rf_oblique |>
  fit(class ~ ., data = cells_subset)
cells_imp_rf_oblique_res@results
#> # A tibble: 5 × 4
#>   name              score outcome predictor     
#>   <chr>             <dbl> <chr>   <chr>         
#> 1 imp_rf_oblique  0.0901  class   avg_inten_ch_1
#> 2 imp_rf_oblique  0.0759  class   avg_inten_ch_2
#> 3 imp_rf_oblique  0.00764 class   area_ch_1     
#> 4 imp_rf_oblique -0.00673 class   avg_inten_ch_3
#> 5 imp_rf_oblique -0.0102  class   angle_ch_1    

# ----------------------------------------------------------------------------

# Random forests for regression task

ames_subset <- modeldata::ames |>
  # Use a small example for efficiency
  dplyr::select(
    Sale_Price,
    MS_SubClass,
    MS_Zoning,
    Lot_Frontage,
    Lot_Area,
    Street
  ) |>
  slice(1:50)
ames_subset <- ames_subset |>
  dplyr::mutate(Sale_Price = log10(Sale_Price))

set.seed(42)
ames_imp_rf_regression_task_res <-
  score_imp_rf |>
  fit(Sale_Price ~ ., data = ames_subset)
ames_imp_rf_regression_task_res@results
#> # A tibble: 5 × 4
#>   name     score outcome    predictor   
#>   <chr>    <dbl> <chr>      <chr>       
#> 1 imp_rf 0.00166 Sale_Price MS_SubClass 
#> 2 imp_rf 0.00117 Sale_Price MS_Zoning   
#> 3 imp_rf 0.0139  Sale_Price Lot_Frontage
#> 4 imp_rf 0.0106  Sale_Price Lot_Area    
#> 5 imp_rf 0       Sale_Price Street      
```
