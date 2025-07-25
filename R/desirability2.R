# ------------------------------------------------------------------------------
#' Show best desirability scores, based on proportion of predictors *(plural)*
#'
#' Analogous to, and adapted from [desirability2::show_best_desirability()] that can
#' simultaneously optimize multiple scores using desirability functions.
#' See [show_best_score_prop()] for *singular* scoring method.
#'
#' @param x A tibble or data frame returned by [fill_safe_values()].

#' @param ... NULL
#' @param prop_terms A numeric value specifying the proportion
#' of predictors to consider.
#'
#' @return Returns a tibble with `prop_terms`
#' proportion of rows. When showing the results,
#' the metrics are presented in "wide format" (one column per metric) and there
#' are new columns for the corresponding desirability values (each starts with
#' `.d_`).
#'
#' @seealso [show_best_desirability_num()], [show_best_desirability_cutoff()]
#'
#' @export
show_best_desirability_prop <- function(
  x,
  ...,
  prop_terms = 0.99
) {
  mtr <- x
  all_vars <- names(mtr)
  res <- desirability2::desirability(..., .use_data = TRUE)
  check_extra_vars(res, all_vars = all_vars)
  mtr <- compute_desirability_scores(res, mtr = mtr)

  mtr <-
    mtr |>
    dplyr::mutate(
      .d_overall = d_overall(dplyr::across(dplyr::starts_with(".d_")))
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
#' See [show_best_desirability_prop()] for details.
#'
#' @inheritParams show_best_desirability_prop
#' @param num_terms A numeric value specifying the number
#' of predictors to consider.
#'
#' @return Returns a tibble with `num_terms`
#' number of rows. When showing the results,
#' the metrics are presented in "wide format" (one column per metric) and there
#' are new columns for the corresponding desirability values (each starts with
#' `.d_`).
#'
#' @seealso [show_best_desirability_prop()], [show_best_desirability_cutoff()]
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
      .d_overall = d_overall(dplyr::across(dplyr::starts_with(".d_")))
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
