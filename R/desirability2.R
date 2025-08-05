# ------------------------------------------------------------------------------
#' Show best desirability scores, based on proportion of predictors *(plural)*
#'
#' Analogous to, and adapted from [desirability2::show_best_desirability()] that can
#' simultaneously optimize multiple scores using desirability functions.
#' See [show_best_score_prop()] for *singular* filtering method.
#'
#' @details Desirability functions might help when selecting the best model
#' based on more than one performance metric. The user creates a desirability
#' function to map values of a metric to a `[0, 1]` range where 1.0 is most
#' desirable and zero is unacceptable. After constructing these for the metric
#' of interest, the overall desirability is computed using the geometric mean
#' of the individual desirabilities.
#'
#' The verbs that can be used in `...` (and their arguments) are:
#'
#' - `maximize()` when larger values are better, such as the area under the ROC
#'    score.
#' - `minimize()` for metrics such as RMSE or the Brier score.
#' - `target()` for cases when a specific value of the metric is important.
#' - `constrain()` is used when there is a range of values that are equally
#'    desirable.
#'
#' @name show_best_desirability_prop
#'
#' @param x A tibble or data frame returned by [fill_safe_values()].
#'
#' @param ... One or more desirability selectors to configure the optimization.
#' @param prop_terms A numeric value specifying the proportion
#' of predictors to consider.
#'
#' @return A tibble with `prop_terms`
#' proportion of rows. When showing the results,
#' the metrics are presented in "wide format" (one column per metric) and there
#' are new columns for the corresponding desirability values (each starts with
#' `.d_`).
#'
#' @examplesIf rlang::is_installed("modeldata")
#'
#' ames_subset <- modeldata::ames |>
#'   # small example for efficiency
#'   dplyr::select(
#'     Sale_Price,
#'     MS_SubClass,
#'     MS_Zoning,
#'     Lot_Frontage,
#'     Lot_Area,
#'     Street
#'   )
#' ames_subset <- ames_subset |>
#'   dplyr::mutate(Sale_Price = log10(Sale_Price))
#'
#' # anova pval
#' ames_aov_pval_res <-
#'   score_aov_pval |>
#'   fit(Sale_Price ~ ., data = ames_subset)
#'
#' # cor
#' ames_cor_pearson_res <-
#'   score_cor_pearson |>
#'   fit(Sale_Price ~ ., data = ames_subset)
#'
#' # forest imp
#' set.seed(42)
#' ames_imp_rf_reg_res <-
#'   score_imp_rf |>
#'   fit(Sale_Price ~ ., data = ames_subset)
#'
#' # info gain
#' score_info_gain_reg <- score_info_gain
#' score_info_gain_reg@mode <- "regression"
#'
#' ames_info_gain_reg_res <-
#'   score_info_gain_reg |>
#'   fit(Sale_Price ~ ., data = ames_subset)
#'
#' # Create a list
#' class_score_list <- list(
#'   ames_aov_pval_res,
#'   ames_cor_pearson_res,
#'   ames_imp_rf_reg_res,
#'   ames_info_gain_reg_res
#' )
#'
#' # Fill safe values
#' scores_combined <- class_score_list |>
#'   filtro::fill_safe_values() |>
#'   dplyr::select(-outcome)
#'
#' library(desirability2)
#'
#' show_best_desirability_prop(
#'   scores_combined,
#'   maximize(cor_pearson, low = 0, high = 1)
#' )
#'
#' show_best_desirability_prop(
#'   scores_combined,
#'   maximize(cor_pearson, low = 0, high = 1),
#'   maximize(imp_rf)
#' )
#'
#' show_best_desirability_prop(
#'   scores_combined,
#'   maximize(cor_pearson, low = 0, high = 1),
#'   maximize(imp_rf),
#'   maximize(infogain)
#' )
#'
#' show_best_desirability_prop(
#'   scores_combined,
#'   maximize(cor_pearson, low = 0, high = 1),
#'   maximize(imp_rf),
#'   maximize(infogain),
#'   prop_terms = 0.2
#' )
#'
#' show_best_desirability_prop(
#'   scores_combined,
#'   target(cor_pearson, low = 0.2, target = 0.255, high = 0.9)
#' )
#'
#' show_best_desirability_prop(
#'   scores_combined,
#'   constrain(cor_pearson, low = 0.2, high = 1)
#' )
#' @export
show_best_desirability_prop <- function(
  x,
  ...,
  prop_terms = 1
) {
  mtr <- x
  all_vars <- names(mtr)
  res <- desirability2::desirability(..., .use_data = TRUE)
  check_extra_vars(res, all_vars = all_vars)
  mtr <- compute_desirability_scores(res, mtr = mtr)

  mtr <-
    mtr |>
    dplyr::mutate(
      .d_overall = desirability2::d_overall(dplyr::across(dplyr::starts_with(
        ".d_"
      )))
    ) |>
    dplyr::slice_max(.d_overall, prop = prop_terms, with_ties = TRUE)
  mtr
}

# ------------------------------------------------------------------------------
#' Show best desirability scores, based on number of predictors *(plural)*
#'
#' Similar to [show_best_desirability_prop()] that can
#' simultaneously optimize multiple scores using desirability functions.
#' See [show_best_score_num()] for *singular* scoring method.
#'
#' @details See [show_best_desirability_prop()] for details.
#'
#' @name show_best_desirability_num
#'
#' @inheritParams show_best_desirability_prop
#' @param num_terms An integer value specifying the number
#' of predictors to consider.
#'
#' @return A tibble with `num_terms`
#' number of rows. When showing the results,
#' the metrics are presented in "wide format" (one column per metric) and there
#' are new columns for the corresponding desirability values (each starts with
#' `.d_`).
#'
#' @export
show_best_desirability_num <- function(
  x,
  ...,
  num_terms = 5
) {
  mtr <- x
  all_vars <- names(mtr)
  res <- desirability2::desirability(..., .use_data = TRUE)
  check_extra_vars(res, all_vars = all_vars)
  mtr <- compute_desirability_scores(res, mtr = mtr)

  mtr <-
    mtr |>
    dplyr::mutate(
      .d_overall = desirability2::d_overall(dplyr::across(dplyr::starts_with(
        ".d_"
      )))
    ) |>
    dplyr::slice_max(.d_overall, n = num_terms, with_ties = TRUE)
  mtr
}

# ------------------------------------------------------------------------------
# Helpers

check_extra_vars <- function(res, all_vars) {
  d_vars <- sort(unique(unlist(res@variables)))
  extra_vars <- setdiff(d_vars, all_vars)
  num_extras <- length(extra_vars)
  if (num_extras > 0) {
    cli::cli_abort(
      "The desirability specification includes {num_extras} variable{?s} that
      {?was/were} not collected: {.val {extra_vars}}."
    )
  }
}

compute_desirability_scores <- function(res, mtr) {
  d_nms <- make_col_names(res)
  for (i in seq_along(res@translated)) {
    tmp <- try(rlang::eval_tidy(res@translated[[i]], mtr), silent = TRUE)
    if (inherits(tmp, "try-error")) {
      cli::cli_abort(
        "An error occured when computing a desirability score: {tmp}."
      )
    }
    mtr[[d_nms[i]]] <- tmp
  }
  mtr
}

make_col_names <- function(x) {
  vars <- purrr::map_chr(x@variables, ~ paste0(.x, collapse = "_"))
  fns <- purrr::map_chr(x@translated, ~ rlang::expr_deparse(.x[[1]]))
  res <- purrr::map2_chr(fns, vars, ~ paste0(".", .x, "_", .y))
  make.names(res, unique = TRUE)
}
