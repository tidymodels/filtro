# Scoring via analysis of variance hypothesis tests

These two objects can be used to compute importance scores based on
Analysis of Variance techniques.

## Usage

``` r
score_aov_pval

score_aov_fstat
```

## Format

An object of class
[`filtro::class_score_aov`](https://filtro.tidymodels.org/dev/reference/class_score.md)
(inherits from
[`filtro::class_score`](https://filtro.tidymodels.org/dev/reference/class_score.md),
`S7_object`) of length 1.

An object of class
[`filtro::class_score_aov`](https://filtro.tidymodels.org/dev/reference/class_score.md)
(inherits from
[`filtro::class_score`](https://filtro.tidymodels.org/dev/reference/class_score.md),
`S7_object`) of length 1.

## Value

An S7 object. The primary property of interest is in `results`. This is
a data frame of results that is populated by the
[`fit()`](https://generics.r-lib.org/reference/fit.html) method and has
columns:

- `name`: The name of the score (e.g., `aov_fstat` or `aov_pval`).

- `score`: The estimates for each predictor.

- `outcome`: The name of the outcome column.

- `predictor`: The names of the predictor inputs.

These data are accessed using `object@results` (see examples below).

## Details

These objects are used when either:

- The predictors are numeric and the outcome is a factor/category, or

- The predictors are factors and the outcome is numeric.

In either case, a linear model (via
[`stats::lm()`](https://rdrr.io/r/stats/lm.html)) is created with the
proper variable roles, and the overall p-value for the hypothesis that
all means are equal is computed via the standard F-statistic. The
p-value that is returned is transformed to be `-log10(p_value)` so that
larger values are associated with more important predictors.

### Estimating the scores

In filtro, the `score_*` objects define a scoring method (e.g., data
input requirements, package dependencies, etc). To compute the scores
for a specific data set, the
[`fit()`](https://generics.r-lib.org/reference/fit.html) method is used.
The main arguments for these functions are:

- `object`:

  A score class object (e.g., `score_aov_pval`).

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
[`score_cor_pearson`](https://filtro.tidymodels.org/dev/reference/score_cor_pearson.md),
[`score_imp_rf`](https://filtro.tidymodels.org/dev/reference/score_imp_rf.md),
[`score_info_gain`](https://filtro.tidymodels.org/dev/reference/score_info_gain.md),
[`score_roc_auc`](https://filtro.tidymodels.org/dev/reference/score_roc_auc.md),
[`score_xtab_pval_chisq`](https://filtro.tidymodels.org/dev/reference/score_xtab_pval_chisq.md)

## Examples

``` r
# Analysis of variance where `class` is the class predictor and the numeric
# predictors are the outcomes/responses

cell_data <- modeldata::cells
cell_data$case <- NULL

# ANOVA p-value
cell_p_val_res <-
  score_aov_pval |>
  fit(class ~ ., data = cell_data)
cell_p_val_res@results
#> # A tibble: 56 × 4
#>    name       score outcome predictor                   
#>    <chr>      <dbl> <chr>   <chr>                       
#>  1 aov_pval  0.0575 class   angle_ch_1                  
#>  2 aov_pval  1.04   class   area_ch_1                   
#>  3 aov_pval 73.2    class   avg_inten_ch_1              
#>  4 aov_pval 88.5    class   avg_inten_ch_2              
#>  5 aov_pval  0.0246 class   avg_inten_ch_3              
#>  6 aov_pval 27.8    class   avg_inten_ch_4              
#>  7 aov_pval 52.6    class   convex_hull_area_ratio_ch_1 
#>  8 aov_pval 60.0    class   convex_hull_perim_ratio_ch_1
#>  9 aov_pval 50.7    class   diff_inten_density_ch_1     
#> 10 aov_pval  1.51   class   diff_inten_density_ch_3     
#> # ℹ 46 more rows

# ANOVA raw p-value
natrual_units <- score_aov_pval |> dont_log_pvalues()
cell_pval_natrual_res <-
  natrual_units |>
  fit(class ~ ., data = cell_data)
cell_pval_natrual_res@results
#> # A tibble: 56 × 4
#>    name        score outcome predictor                   
#>    <chr>       <dbl> <chr>   <chr>                       
#>  1 aov_pval 8.76e- 1 class   angle_ch_1                  
#>  2 aov_pval 9.05e- 2 class   area_ch_1                   
#>  3 aov_pval 6.02e-74 class   avg_inten_ch_1              
#>  4 aov_pval 3.02e-89 class   avg_inten_ch_2              
#>  5 aov_pval 9.45e- 1 class   avg_inten_ch_3              
#>  6 aov_pval 1.47e-28 class   avg_inten_ch_4              
#>  7 aov_pval 2.63e-53 class   convex_hull_area_ratio_ch_1 
#>  8 aov_pval 1.08e-60 class   convex_hull_perim_ratio_ch_1
#>  9 aov_pval 1.90e-51 class   diff_inten_density_ch_1     
#> 10 aov_pval 3.07e- 2 class   diff_inten_density_ch_3     
#> # ℹ 46 more rows

# ANOVA t/F-statistic
cell_t_stat_res <-
  score_aov_fstat |>
  fit(class ~ ., data = cell_data)
cell_t_stat_res@results
#> # A tibble: 56 × 4
#>    name          score outcome predictor                   
#>    <chr>         <dbl> <chr>   <chr>                       
#>  1 aov_fstat   0.0244  class   angle_ch_1                  
#>  2 aov_fstat   2.87    class   area_ch_1                   
#>  3 aov_fstat 360.      class   avg_inten_ch_1              
#>  4 aov_fstat 444.      class   avg_inten_ch_2              
#>  5 aov_fstat   0.00477 class   avg_inten_ch_3              
#>  6 aov_fstat 127.      class   avg_inten_ch_4              
#>  7 aov_fstat 251.      class   convex_hull_area_ratio_ch_1 
#>  8 aov_fstat 289.      class   convex_hull_perim_ratio_ch_1
#>  9 aov_fstat 241.      class   diff_inten_density_ch_1     
#> 10 aov_fstat   4.68    class   diff_inten_density_ch_3     
#> # ℹ 46 more rows

# ---------------------------------------------------------------------------
library(dplyr)

# Analysis of variance where `chem_fp_*` are the class predictors and
# `permeability` is the numeric outcome/response

permeability <-
  modeldata::permeability_qsar |>
  # Make the problem a little smaller for time; use 50 predictors
  select(1:51) |>
  # Make the binary predictor columns into factors
  mutate(across(starts_with("chem_fp"), as.factor))

perm_p_val_res <-
  score_aov_pval |>
  fit(permeability ~ ., data = permeability)
perm_p_val_res@results
#> # A tibble: 50 × 4
#>    name      score outcome      predictor   
#>    <chr>     <dbl> <chr>        <chr>       
#>  1 aov_pval  1.88  permeability chem_fp_0001
#>  2 aov_pval  1.63  permeability chem_fp_0002
#>  3 aov_pval  1.36  permeability chem_fp_0003
#>  4 aov_pval  1.36  permeability chem_fp_0004
#>  5 aov_pval  1.36  permeability chem_fp_0005
#>  6 aov_pval 10.6   permeability chem_fp_0006
#>  7 aov_pval NA     permeability chem_fp_0007
#>  8 aov_pval NA     permeability chem_fp_0008
#>  9 aov_pval  0.265 permeability chem_fp_0009
#> 10 aov_pval  0.341 permeability chem_fp_0010
#> # ℹ 40 more rows

# Note that some `lm()` calls failed and are given NA score values. For
# example:
table(permeability$chem_fp_0007)
#> 
#>   1 
#> 165 

perm_t_stat_res <-
  score_aov_fstat |>
  fit(permeability ~ ., data = permeability)
perm_t_stat_res@results
#> # A tibble: 50 × 4
#>    name       score outcome      predictor   
#>    <chr>      <dbl> <chr>        <chr>       
#>  1 aov_fstat  6.28  permeability chem_fp_0001
#>  2 aov_fstat  5.22  permeability chem_fp_0002
#>  3 aov_fstat  4.13  permeability chem_fp_0003
#>  4 aov_fstat  4.13  permeability chem_fp_0004
#>  5 aov_fstat  4.13  permeability chem_fp_0005
#>  6 aov_fstat 51.3   permeability chem_fp_0006
#>  7 aov_fstat NA     permeability chem_fp_0007
#>  8 aov_fstat NA     permeability chem_fp_0008
#>  9 aov_fstat  0.371 permeability chem_fp_0009
#> 10 aov_fstat  0.559 permeability chem_fp_0010
#> # ℹ 40 more rows
```
