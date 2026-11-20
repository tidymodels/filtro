# Scoring via the chi-squared test or Fisher's exact test

These two objects can be used to compute importance scores based on
chi-squared test or Fisher's exact test.

## Usage

``` r
score_xtab_pval_chisq

score_xtab_pval_fisher
```

## Format

An object of class
[`filtro::class_score_xtab`](https://filtro.tidymodels.org/dev/reference/class_score.md)
(inherits from
[`filtro::class_score`](https://filtro.tidymodels.org/dev/reference/class_score.md),
`S7_object`) of length 1.

An object of class
[`filtro::class_score_xtab`](https://filtro.tidymodels.org/dev/reference/class_score.md)
(inherits from
[`filtro::class_score`](https://filtro.tidymodels.org/dev/reference/class_score.md),
`S7_object`) of length 1.

## Value

An S7 object. The primary property of interest is in `results`. This is
a data frame of results that is populated by the
[`fit()`](https://generics.r-lib.org/reference/fit.html) method and has
columns:

- `name`: The name of the score (e.g., `pval_chisq`).

- `score`: The estimates for each predictor.

- `outcome`: The name of the outcome column.

- `predictor`: The names of the predictor inputs.

These data are accessed using `object@results` (see examples below).

## Details

These objects are used when:

- The predictors are factors and the outcome is a factor.

In this case, a contingency table (via
[`table()`](https://rdrr.io/r/base/table.html)) is created with the
proper variable roles, and the cross tabulation p-value is computed
using either the chi-squared test (via
[`stats::chisq.test()`](https://rdrr.io/r/stats/chisq.test.html)) or
Fisher's exact test (via
[`stats::fisher.test()`](https://rdrr.io/r/stats/fisher.test.html)). The
p-value that is returned is transformed to be `-log10(p_value)` so that
larger values are associated with more important predictors.

### Estimating the scores

In filtro, the `score_*` objects define a scoring method (e.g., data
input requirements, package dependencies, etc). To compute the scores
for a specific data set, the
[`fit()`](https://generics.r-lib.org/reference/fit.html) method is used.
The main arguments for these functions are:

- `object`:

  A score class object (e.g., `score_xtab_pval_chisq`).

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
[`score_info_gain`](https://filtro.tidymodels.org/dev/reference/score_info_gain.md),
[`score_roc_auc`](https://filtro.tidymodels.org/dev/reference/score_roc_auc.md)

## Examples

``` r
# Binary factor example

library(titanic)
library(dplyr)

titanic_subset <- titanic_train |>
  mutate(across(c(Survived, Pclass, Sex, Embarked), as.factor)) |>
  select(Survived, Pclass, Sex, Age, Fare, Embarked)

# Chi-squared test
titanic_xtab_pval_chisq_res <- score_xtab_pval_chisq |>
  fit(Survived ~ ., data = titanic_subset)
titanic_xtab_pval_chisq_res@results
#> # A tibble: 5 × 4
#>   name            score outcome  predictor
#>   <chr>           <dbl> <chr>    <chr>    
#> 1 xtab_pval_chisq 22.3  Survived Pclass   
#> 2 xtab_pval_chisq 57.9  Survived Sex      
#> 3 xtab_pval_chisq NA    Survived Age      
#> 4 xtab_pval_chisq NA    Survived Fare     
#> 5 xtab_pval_chisq  5.79 Survived Embarked 

# Chi-squared test adjusted p-values
titanic_xtab_pval_chisq_p_adj_res <- score_xtab_pval_chisq |>
  fit(Survived ~ ., data = titanic_subset, adjustment = "BH")

# Fisher's exact test
titanic_xtab_pval_fisher_res <- score_xtab_pval_fisher |>
  fit(Survived ~ ., data = titanic_subset)
titanic_xtab_pval_fisher_res@results
#> # A tibble: 5 × 4
#>   name             score outcome  predictor
#>   <chr>            <dbl> <chr>    <chr>    
#> 1 xtab_pval_fisher 22.5  Survived Pclass   
#> 2 xtab_pval_fisher 59.2  Survived Sex      
#> 3 xtab_pval_fisher NA    Survived Age      
#> 4 xtab_pval_fisher NA    Survived Fare     
#> 5 xtab_pval_fisher  5.99 Survived Embarked 

# Chi-squared test where `class` is the multiclass outcome/response

hpc_subset <- modeldata::hpc_data |>
  dplyr::select(
    class,
    protocol,
    hour
  )

hpc_xtab_pval_chisq_res <- score_xtab_pval_chisq |>
    fit(class ~ ., data = hpc_subset)
hpc_xtab_pval_chisq_res@results
#> # A tibble: 2 × 4
#>   name             score outcome predictor
#>   <chr>            <dbl> <chr>   <chr>    
#> 1 xtab_pval_chisq  0.169 class   protocol 
#> 2 xtab_pval_chisq NA     class   hour     
```
