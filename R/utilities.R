# ------------------------------------------------------------------------------
#' @keywords internal
#' @export
arrange_score <- S7::new_generic(
  "arrange_score",
  dispatch_args = "x",
  function(x, ...) {
    if (!S7::S7_inherits(x, class_score)) {
      cli::cli_abort(
        "{.arg x} must be a {.cls class_score}, not {.obj_type_friendly {x}}."
      )
    }

    S7::S7_dispatch()
  }
)

#' Arrange score
#'
#' @name arrange_score
#'
#' @param x A score class object (e.g., `score_cor_pearson`).
#'
#' @param ... Further arguments passed to or from other methods.
#' @param target A numeric value specifying the target value. The default
#' of `NULL` indicates that there is no target value.
#'
#' @return A tibble of score results.
#'
#' @examplesIf rlang::is_installed("modeldata")
#'
#' library(dplyr)
#'
#' ames_subset <- modeldata::ames |>
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
#' ames_aov_pval_res <-
#'   score_aov_pval |>
#'   fit(Sale_Price ~ ., data = ames_subset)
#' ames_aov_pval_res@results
#'
#' # Arrange score
#' ames_aov_pval_res |> arrange_score()
#'
#' @export
S7::method(arrange_score, class_score) <- function(x, ..., target = NULL) {
  if (x@direction == "maximize") {
    x@results |> dplyr::arrange(dplyr::desc(score))
  } else if (x@direction == "minimize") {
    x@results |> dplyr::arrange(score)
  } else if (x@direction == "target") {
    check_target(target)
    x@results |> dplyr::arrange(abs(score - target))
  }
}

# ------------------------------------------------------------------------------
#' @keywords internal
#' @export
fill_safe_value <- S7::new_generic("fill_safe_value", dispatch_args = "x")

#' Fill safe value *(singular)*
#'
#' Fills in safe value for missing score, with an option to apply transformation.
#' This is a *singular* scoring method. See [fill_safe_values()] for *plural* scoring method.
#'
#' @name fill_safe_value
#'
#' @details
#' If `transform = TRUE`, by default, all score objects use the identity transformation, except the
#' correlation score object, which uses the absolute transformation.
#'
#' @param x A score class object (e.g., `score_cor_pearson`).
#'
#' @param return_results A logical value indicating whether to return results.
#'
#' @return A tibble of score results.
#'
#' @examplesIf rlang::is_installed("modeldata")
#'
#' library(dplyr)
#'
#' ames_subset <- modeldata::ames |>
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
#' ames_aov_pval_res <-
#'   score_aov_pval |>
#'   fit(Sale_Price ~ ., data = ames_subset)
#' ames_aov_pval_res@results
#'
#' # Fill safe value
#' ames_aov_pval_res |> fill_safe_value(return_results = TRUE)
#'
#' # Fill safe value, option to transform
#' ames_aov_pval_res |> fill_safe_value(return_results = TRUE, transform = TRUE)
#'
#' @export
S7::method(fill_safe_value, class_score) <- function(
  x,
  return_results = FALSE,
  transform = FALSE
) {
  results <- x@results
  is_na_score <- is.na(results$score)
  results$score[is_na_score] <- x@fallback_value

  if (transform && !is.null(x@transform_fn)) {
    results$score <- x@transform_fn(results$score)
  }

  x@results <- results

  if (return_results) {
    x@results
  } else {
    x
  }
}

#' @keywords internal
#' @export
show_best_score_prop <- S7::new_generic(
  "show_best_score_prop",
  dispatch_args = "x",
  function(x, ...) {
    if (!S7::S7_inherits(x, class_score)) {
      cli::cli_abort(
        "{.arg x} must be a {.cls class_score}, not {.obj_type_friendly {x}}."
      )
    }

    S7::S7_dispatch()
  }
)

# ------------------------------------------------------------------------------
#' Show best score, based on proportion of predictors *(singular)*
#'
#' @name show_best_score_prop
#'
#' @param x A score class object (e.g., `score_cor_pearson`).
#'
#' @param ... Further arguments passed to or from other methods.
#' @param prop_terms A numeric value specifying the proportion
#' of predictors to consider.
#'
#' @return A tibble of score results.
#'
#' @examplesIf rlang::is_installed("modeldata")
#'
#' library(dplyr)
#'
#' ames_subset <- modeldata::ames |>
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
#' ames_aov_pval_res <-
#'   score_aov_pval |>
#'   fit(Sale_Price ~ ., data = ames_subset)
#' ames_aov_pval_res@results
#'
#' # Show best score
#' ames_aov_pval_res |> show_best_score_prop(prop_terms = 0.2)
#'
#' @export
S7::method(show_best_score_prop, class_score) <- function(
  x,
  ...,
  prop_terms,
  target = NULL
) {
  check_prop_terms(prop_terms)

  # TODO Can return NULL for prop = 0.1 if # of predictor is small. dplyr::near()?
  if (x@direction == "maximize") {
    x@results |> dplyr::slice_max(score, prop = prop_terms)
  } else if (x@direction == "minimize") {
    x@results |> dplyr::slice_min(score, prop = prop_terms)
  } else if (x@direction == "target") {
    check_target(target)
    x@results |>
      dplyr::arrange(abs(score - target)) |>
      dplyr::slice_head(prop = prop_terms)
  }
}

#' @keywords internal
#' @export
show_best_score_num <- S7::new_generic(
  "show_best_score_num",
  dispatch_args = "x",
  function(x, ...) {
    if (!S7::S7_inherits(x, class_score)) {
      cli::cli_abort(
        "{.arg x} must be a {.cls class_score}, not {.obj_type_friendly {x}}."
      )
    }

    S7::S7_dispatch()
  }
)

# ------------------------------------------------------------------------------
#' Show best score, based on number of predictors *(singular)*
#'
#' @name show_best_score_num
#'
#' @param x A score class object (e.g., `score_cor_pearson`).
#'
#' @param ... Further arguments passed to or from other methods.
#' @param num_terms An integer value specifying the number
#' of predictors to consider.
#'
#' @return A tibble of score results.
#'
#' @examplesIf rlang::is_installed("modeldata")
#'
#' library(dplyr)
#'
#' ames_subset <- modeldata::ames |>
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
#' ames_aov_pval_res <-
#'   score_aov_pval |>
#'   fit(Sale_Price ~ ., data = ames_subset)
#' ames_aov_pval_res@results
#'
#' # Show best score
#' ames_aov_pval_res |> show_best_score_num(num_terms = 2)
#'
#' @export
S7::method(show_best_score_num, class_score) <- function(
  x,
  ...,
  num_terms,
  target = NULL
) {
  check_num_terms(num_terms)

  if (x@direction == "maximize") {
    x@results |> dplyr::slice_max(score, n = num_terms)
  } else if (x@direction == "minimize") {
    x@results |> dplyr::slice_min(score, n = num_terms)
  } else if (x@direction == "target") {
    check_target(target)
    x@results |>
      dplyr::arrange(abs(score - target)) |>
      dplyr::slice_head(n = num_terms)
  }
}

# ------------------------------------------------------------------------------
#' @keywords internal
#' @export
show_best_score_cutoff <- S7::new_generic(
  "show_best_score_cutoff",
  dispatch_args = "x",
  function(x, ...) {
    if (!S7::S7_inherits(x, class_score)) {
      cli::cli_abort(
        "{.arg x} must be a {.cls class_score}, not {.obj_type_friendly {x}}."
      )
    }

    S7::S7_dispatch()
  }
)

#' Show best score, based on based on cutoff value *(singular)*
#'
#' @name show_best_score_cutoff
#'
#' @param x A score class object (e.g., `score_cor_pearson`).
#'
#' @param ... Further arguments passed to or from other methods.
#' @param cutoff A numeric value specifying the cutoff value.
#' @param target A numeric value specifying the target value. The default
#' of `NULL` indicates that there is no target value.
#'
#' @return A tibble of score results.
#'
#' @examplesIf rlang::is_installed("modeldata")
#'
#' library(dplyr)
#'
#' ames_subset <- modeldata::ames |>
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
#' ames_aov_pval_res <-
#'   score_aov_pval |>
#'   fit(Sale_Price ~ ., data = ames_subset)
#' ames_aov_pval_res@results
#'
#' # Show best score
#' ames_aov_pval_res |> show_best_score_cutoff(cutoff = 130)
#'
#' @export
S7::method(show_best_score_cutoff, class_score) <- function(
  x,
  ...,
  cutoff,
  target = NULL
) {
  check_cutoff(cutoff)

  # TODO Can return more # of predictors due to floating-point precision
  if (x@direction == "maximize") {
    x@results |> dplyr::filter(score >= cutoff)
  } else if (x@direction == "minimize") {
    x@results |> dplyr::filter(score <= cutoff)
  } else if (x@direction == "target") {
    # TODO cutoff = is now based on abs(). Not ideal?
    check_target(target)
    x@results |> dplyr::filter(abs(score - target) <= cutoff)
  }
}

# ------------------------------------------------------------------------------
#' @keywords internal
#' @export
show_best_score_dual <- S7::new_generic(
  "show_best_score_dual",
  dispatch_args = "x",
  function(x, ...) {
    if (!S7::S7_inherits(x, class_score)) {
      cli::cli_abort(
        "{.arg x} must be a {.cls class_score}, not {.obj_type_friendly {x}}."
      )
    }

    S7::S7_dispatch()
  }
)

#' Show best score, based on number or proportion of predictors with
#' optional cutoff value *(singular)*
#'
#' @name show_best_score_dual
#'
#' @param x A score class object (e.g., `score_cor_pearson`).
#'
#' @param ... Further arguments passed to or from other methods.
#' @param prop_terms A numeric value specifying the proportion
#' of predictors to consider.
#' @param num_terms An integer value specifying the number
#' of predictors to consider.
#' @param cutoff A numeric value specifying the cutoff value.
#'
#' @return A tibble of score results.
#'
#' @examplesIf rlang::is_installed("modeldata")
#'
#' library(dplyr)
#'
#' ames_subset <- modeldata::ames |>
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
#' ames_aov_pval_res <-
#'   score_aov_pval |>
#'   fit(Sale_Price ~ ., data = ames_subset)
#' ames_aov_pval_res@results
#'
#' # Show best score
#' ames_aov_pval_res |> show_best_score_dual(prop_terms = 0.5)
#' ames_aov_pval_res |> show_best_score_dual(prop_terms = 0.5, cutoff = 130)
#'
#' ames_aov_pval_res |> show_best_score_dual(num_terms = 2)
#' ames_aov_pval_res |> show_best_score_dual(num_terms = 2, cutoff = 130)
#'
#' @export
S7::method(show_best_score_dual, class_score) <- function(
  x,
  ...,
  num_terms = NULL,
  prop_terms = NULL,
  cutoff = NULL
) {
  if (!is.null(num_terms) && !is.null(prop_terms)) {
    cli::cli_abort(
      "{.arg num_terms} and {.arg prop_terms} are mutually exclusive."
    )
  }

  if (!is.null(num_terms)) {
    results <- show_best_score_num(x, ..., num_terms = num_terms)
  } else if (!is.null(prop_terms)) {
    results <- show_best_score_prop(
      x,
      ...,
      prop_terms = prop_terms
    )
  }
  if (!is.null(cutoff)) {
    results <- show_best_score_cutoff(x, ..., cutoff = cutoff)
  }
  results
}

# ------------------------------------------------------------------------------
#' Rank score based on `dplyr::min_rank()`, where tied values receive the
#' same rank and ranks are with gaps *(singular)*
#'
#' @param x A score class object (e.g., `score_cor_pearson`).
#'
#' @param ... Further arguments passed to or from other methods.
#'
#' @return A tibble of score results.
#'
#' @examplesIf rlang::is_installed("modeldata")
#'
#' library(dplyr)
#'
#' ames_subset <- modeldata::ames |>
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
#' ames_aov_pval_res <-
#'   score_aov_pval |>
#'   fit(Sale_Price ~ ., data = ames_subset)
#' ames_aov_pval_res@results
#'
#' # Rank score
#' ames_aov_pval_res |> rank_best_score_min()
#'
#' @export
rank_best_score_min <- S7::new_generic(
  "rank_best_score_min",
  dispatch_args = "x",
  function(x, ...) {
    if (!S7::S7_inherits(x, class_score)) {
      cli::cli_abort(
        "{.arg x} must be a {.cls class_score}, not {.obj_type_friendly {x}}."
      )
    }

    S7::S7_dispatch()
  }
)

#' @noRd
#' @export
S7::method(rank_best_score_min, class_score) <- function(
  x,
  ...,
  target = NULL
) {
  # TODO Check if direction == target, add "need a target"

  # TODO Need to deal with NA
  if (x@direction == "maximize") {
    x@results |> dplyr::mutate(rank = dplyr::min_rank((dplyr::desc(score))))
  } else if (x@direction == "minimize") {
    x@results |> dplyr::mutate(rank = dplyr::min_rank((score)))
  } # else if (x@direction == "target") { # TODO
  #   x@results |> dplyr::arrange(abs(score - target))
  # }
}

# ------------------------------------------------------------------------------
#' Rank score based on `dplyr::dense_rank()`, where tied values receive the
#' same rank and ranks are without gaps *(singular)*
#'
#' @param x A score class object (e.g., `score_cor_pearson`).
#'
#' @param ... Further arguments passed to or from other methods.
#'
#' @return A tibble of score results.
#'
#' @examplesIf rlang::is_installed("modeldata")
#'
#' library(dplyr)
#'
#' ames_subset <- modeldata::ames |>
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
#' ames_aov_pval_res <-
#'   score_aov_pval |>
#'   fit(Sale_Price ~ ., data = ames_subset)
#' ames_aov_pval_res@results
#'
#' # Rank score
#' ames_aov_pval_res |> rank_best_score_dense()
#'
#' @export
rank_best_score_dense <- S7::new_generic(
  "rank_best_score_dense",
  dispatch_args = "x",
  function(x, ...) {
    if (!S7::S7_inherits(x, class_score)) {
      cli::cli_abort(
        "{.arg x} must be a {.cls class_score}, not {.obj_type_friendly {x}}."
      )
    }

    S7::S7_dispatch()
  }
)

#' @noRd
#' @export
S7::method(rank_best_score_dense, class_score) <- function(
  x,
  ...,
  target = NULL
) {
  # TODO Check if direction == target, add "need a target"

  # TODO Need to deal with NA
  if (x@direction == "maximize") {
    x@results |> dplyr::mutate(rank = dplyr::dense_rank((dplyr::desc(score))))
  } else if (x@direction == "minimize") {
    x@results |> dplyr::mutate(rank = dplyr::dense_rank((score)))
  } # else if (x@direction == "target") { # TODO
  #   x@results |> dplyr::arrange(abs(score - target))
  # }
}

# ------------------------------------------------------------------------------
#' S7 subclass of base R's `list` for method dispatch
#'
#' `class_score_list` is an S7 subclass of S3 base R's `list`, used for method dispatch in
#' [bind_scores()] and [fill_safe_values()].
#'
#' @return A list of S7 objects.
#'
#' @examplesIf rlang::is_installed("modeldata")
#'
#' library(dplyr)
#'
#' ames_subset <- modeldata::ames |>
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
#' # ANOVA p-value
#' ames_aov_pval_res <-
#'   score_aov_pval |>
#'   fit(Sale_Price ~ ., data = ames_subset)
#' ames_aov_pval_res@results
#'
#' # Pearson correlation
#' ames_cor_pearson_res <-
#'   score_cor_pearson |>
#'   fit(Sale_Price ~ ., data = ames_subset)
#' ames_cor_pearson_res@results
#'
#' # Create a list
#' class_score_list <- list(
#'   ames_aov_pval_res,
#'   ames_cor_pearson_res
#' )
#'
#' @export
class_score_list <- S7::new_S3_class("list")

# ------------------------------------------------------------------------------
#' @keywords internal
#' @export
bind_scores <- S7::new_generic("bind_scores", dispatch_args = "x")

#' Bind score class object, including their associated metadata and scores
#'
#' Binds multiple score class objects (e.g., `score_*`), including their associated metadata and scores.
#' See [fill_safe_values()] for binding with safe-value handling.
#'
#' @name bind_scores
#'
#' @param x A list.
#'
#' @param ... Further arguments passed to or from other methods.
#'
#' @return A tibble of scores results.
#'
#' @examplesIf rlang::is_installed("modeldata")
#'
#' library(dplyr)
#'
#' ames_subset <- modeldata::ames |>
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
#' # ANOVA p-value
#' ames_aov_pval_res <-
#'   score_aov_pval |>
#'   fit(Sale_Price ~ ., data = ames_subset)
#' ames_aov_pval_res@results
#'
#' # Pearson correlation
#' ames_cor_pearson_res <-
#'   score_cor_pearson |>
#'   fit(Sale_Price ~ ., data = ames_subset)
#' ames_cor_pearson_res@results
#'
#' # Forest importance
#' set.seed(42)
#' ames_imp_rf_reg_res <-
#'   score_imp_rf |>
#'   fit(Sale_Price ~ ., data = ames_subset)
#' ames_imp_rf_reg_res@results
#'
#' # Information gain
#' ames_info_gain_reg_res <-
#'   score_info_gain |>
#'   fit(Sale_Price ~ ., data = ames_subset)
#' ames_info_gain_reg_res@results
#'
#' # Create a list
#' class_score_list <- list(
#'   ames_aov_pval_res,
#'   ames_cor_pearson_res,
#'   ames_imp_rf_reg_res,
#'   ames_info_gain_reg_res
#' )
#'
#' # Bind scores
#' class_score_list |> bind_scores()
#'
#' @export
S7::method(bind_scores, class_score_list) <- function(x) {
  length_x <- length(x)
  if (length_x == 0) {
    cli::cli_abort(
      "{.arg x} must contain at least one element"
    )
  }
  score_set <- x[[1]]@results
  if (length_x > 1) {
    # TODO Check for identical score object, e.g., list(score_obj_aov, score_obj_aov)
    for (i in 2:length_x) {
      score_set <- dplyr::full_join(
        score_set,
        x[[i]]@results,
        by = c("name", "score", "outcome", "predictor")
      )
    }
  }

  score_set <- score_set |>
    tidyr::pivot_wider(names_from = name, values_from = score)
  score_set
}

# ------------------------------------------------------------------------------
#' @keywords internal
#' @export
fill_safe_values <- S7::new_generic("fill_safe_values", dispatch_args = "x")

#' Fill safe values *(plural)*
#'
#' Wraps [bind_scores()], and fills in safe values for missing scores, with an option to apply transformation.
#' This is a *plural* scoring method. See [fill_safe_value()] for *singular* scoring method.
#'
#' @name fill_safe_values
#'
#' @details
#' If `transform = TRUE`, by default, all score objects use the identity transformation, except the
#' correlation score object, which uses the absolute transformation.
#'
#' @param x A list.
#'
#' @param ... Further arguments passed to or from other methods.
#'
#' @return A tibble of scores results.
#'
#' @examplesIf rlang::is_installed("modeldata")
#'
#' library(dplyr)
#'
#' ames_subset <- modeldata::ames |>
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
#' # ANOVA p-value
#' ames_aov_pval_res <-
#'   score_aov_pval |>
#'   fit(Sale_Price ~ ., data = ames_subset)
#' ames_aov_pval_res@results
#'
#' # Pearson correlation
#' ames_cor_pearson_res <-
#'   score_cor_pearson |>
#'   fit(Sale_Price ~ ., data = ames_subset)
#' ames_cor_pearson_res@results
#'
#' # Forest importance
#' set.seed(42)
#' ames_imp_rf_reg_res <-
#'   score_imp_rf |>
#'   fit(Sale_Price ~ ., data = ames_subset)
#' ames_imp_rf_reg_res@results
#'
#' # Information gain
#' ames_info_gain_reg_res <-
#'   score_info_gain |>
#'   fit(Sale_Price ~ ., data = ames_subset)
#' ames_info_gain_reg_res@results
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
#' class_score_list |> fill_safe_values()
#'
#' # Fill safe value, option to transform
#' class_score_list |> fill_safe_values(transform = TRUE)
#'
#' @export
S7::method(fill_safe_values, class_score_list) <- function(
  x,
  transform = FALSE
) {
  # TODO Max was saying maybe we can fill safe value as we merge in (PR #33)
  score_set <- bind_scores(x)
  for (i in 1:length(x)) {
    method_name <- x[[i]]@score_type
    fallback_val <- x[[i]]@fallback_value
    is_na_score <- is.na(score_set[[method_name]])
    score_set[[method_name]][is_na_score] <- fallback_val

    if (transform && !is.null(x[[i]]@transform_fn)) {
      score_set[[method_name]] <- x[[i]]@transform_fn(score_set[[method_name]])
    }
  }
  score_set
}

# TODO Drop outcome column

# ------------------------------------------------------------------------------
# Used for transformation for fill safe value and fill safe values

filtro_abs_trans <- function(x) {
  abs(x)
}

filtro_pmax_trans <- function(x) {
  pmax(x, 1 - x)
}

# ------------------------------------------------------------------------------
# Used with ANOVA methods

flip_if_needed_aov <- function(predictor, outcome) {
  if (is.factor(outcome) && is.numeric(predictor)) {
    list(predictor = outcome, outcome = predictor)
  } else {
    list(predictor = predictor, outcome = outcome)
  }
}

#' Disable -log10 transformation of p-values
#'
#' @name dont_log_pvalues
#'
#' @param x A score class object.
#'
#' @return The modified score class object with `neg_log10` set to `FALSE`.
#'
#' @export
dont_log_pvalues <- function(x) {
  x@neg_log10 <- FALSE
  x
}

# ------------------------------------------------------------------------------
# Used with ROC AUC methods

flip_if_needed_roc_auc <- function(predictor, outcome) {
  if (is.factor(outcome) && is.numeric(predictor)) {
    list(predictor = predictor, outcome = outcome)
  } else {
    list(predictor = outcome, outcome = predictor)
  }
}

# ------------------------------------------------------------------------------
# lightly process entire data setdata

# Add args for data types and filter here?
process_all_data <- function(f, data) {
  # Note that model.frame() places the outcome(s) as the first column(s)
  mod_frame <- stats::model.frame(
    f,
    data = data,
    drop.unused.levels = TRUE,
    # We can omit data later (if needed) on a one-predictor-at-a-time basis
    na.action = "na.pass"
  )

  res <- tibble::as_tibble(mod_frame)

  res
}

# ------------------------------------------------------------------------------
# reshape data

named_vec_to_tibble <- function(x, name, outcome) {
  res <- tibble::enframe(x, name = "predictor", value = "score") |>
    dplyr::mutate(outcome = outcome, name = name)
  res[, c("name", "score", "outcome", "predictor")]
}

# ------------------------------------------------------------------------------
# Case weight helpers

convert_weights <- function(weights, num_rows, call = rlang::caller_env()) {
  if (is.null(weights)) {
    return(weights)
  }
  if (!is.numeric(weights)) {
    cli::cli_abort(
      "{.arg case_weights} should be a numeric or case weight vector, not
      {.obj_type_friendly {weights}}",
      call = call
    )
  }

  if (length(weights) != num_rows) {
    cli::cli_abort(
      "There should be as many values in {.arg case_weights} ({length(weights)})
      as there are rows in `data` ({num_rows}).",
      call = call
    )
  }
  # in case the class is importance_weights or frequency weights, strip their
  # extra class(es) and attributes
  as.numeric(weights)
}

# The first column is assumed to be the outcome
find_zero_variance_cols <- function(data) {
  is_zv <- purrr::map_lgl(data, \(x) vctrs::vec_unique_count(x) == 1L)
  if (is_zv[1]) {
    cli::cli_abort(
      "The outcome column {.val {names(is_zv)[1]}} has zero variance and
      cannot be used.",
      call = NULL
    )
  }

  if (all(is_zv[-1])) {
    cli::cli_abort(
      "All of the predictors have zero variance and cannot be used.",
      call = NULL
    )
  }

  names(is_zv)[is_zv]
}
