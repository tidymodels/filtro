# Changelog

## filtro (development version)

- Updates for compatibility with S7 \> 0.2.2.

## filtro 0.2.0

CRAN release: 2025-08-26

- Added five new scoring methods: correlation, random forest feature
  importance, information gain, area under the ROC curve, and cross
  tabulation filters.

- Transitioned from S3 to S7 and refactored S7 for improved clarity,
  maintainability, and performance.

- Added multiple new functions to support ranking of single score.
  [`show_best_score_prop()`](https://filtro.tidymodels.org/dev/reference/show_best_score_prop.md),
  [`show_best_score_num()`](https://filtro.tidymodels.org/dev/reference/show_best_score_num.md),
  [`show_best_score_cutoff()`](https://filtro.tidymodels.org/dev/reference/show_best_score_cutoff.md),
  [`show_best_score_dual()`](https://filtro.tidymodels.org/dev/reference/show_best_score_dual.md),
  [`rank_best_score_min()`](https://filtro.tidymodels.org/dev/reference/rank_best_score_min.md)
  and
  [`rank_best_score_dense()`](https://filtro.tidymodels.org/dev/reference/rank_best_score_dense.md)
  can be used to select and rank single score.

- Added two new functions to support ranking of multiple scores.
  [`show_best_desirability_prop()`](https://filtro.tidymodels.org/dev/reference/show_best_desirability_prop.md)
  and
  [`show_best_desirability_num()`](https://filtro.tidymodels.org/dev/reference/show_best_desirability_num.md)
  can be used for multiparameter optimization via desirability
  functions.

- Added multiple utility functions to support handling of both single
  and multiple scores:
  [`arrange_score()`](https://filtro.tidymodels.org/dev/reference/arrange_score.md),
  [`fill_safe_value()`](https://filtro.tidymodels.org/dev/reference/fill_safe_value.md),
  [`bind_scores()`](https://filtro.tidymodels.org/dev/reference/bind_scores.md),
  and
  [`fill_safe_values()`](https://filtro.tidymodels.org/dev/reference/fill_safe_values.md).

## filtro 0.1.0

CRAN release: 2025-07-18

- Initial CRAN submission.
