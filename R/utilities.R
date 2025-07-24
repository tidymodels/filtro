# ------------------------------------------------------------------------------
#' Arrange score
#'
#' @param x A score class object.
#'
#' @param ... NULL
#'
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

#' @noRd
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

# # ------------------------------------------------------------------------------
# #' Transform score
# #'
# #' @param x A score class object.
# #'
# #' @param ... NULL
# #'
# #' @export
# transform_score <- S7::new_generic(
#   "transform_score",
#   dispatch_args = "x",
#   function(x, ...) {
#     if (!S7::S7_inherits(x, class_score)) {
#       cli::cli_abort(
#         "{.arg x} must be a {.cls class_score}, not {.obj_type_friendly {x}}."
#       )
#     }

#     S7::S7_dispatch()
#   }
# )

# S7::method(transform_score, class_score) <- function(x, ...) {
#   if (is.null(x@trans)) {
#     trans <- scales::transform_identity()
#   } else {
#     trans <- x@trans
#   }
#   x@results |>
#     dplyr::mutate(score = trans$transform(score))
# }

# ------------------------------------------------------------------------------
#' Show best score based on proportion of predictors
#'
#' @param x A score class object.
#'
#' @param ... NULL
#'
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

#' @noRd
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


# ------------------------------------------------------------------------------
#' Show best score based on number of predictors
#'
#' @param x A score class object.
#'
#' @param ... NULL
#'
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

#' @noRd
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
#' Show best score based on based on cutoff value
#'
#' @param x A score class object.
#'
#' @param ... NULL
#'
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

#' @noRd
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
#' Show best score based on number or proportion of predictors with
#' optional cutoff value
#'
#' @param x A score class object.
#'
#' @param ... NULL
#'
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

#' @noRd
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
#' Rank score based on min_rank(), where tied values receive the same (smalles) rank
#' and ranks are with gaps
#'
#' @param x A score class object.
#'
#' @param ... NULL
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

#' Rank score based on dense_rank(), where tied values receive the same rank
#' and ranks are consecutive without gaps
#'
#' @param x A score class object.
#'
#' @param ... NULL
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
#' Construct an S7 subclass of base R's `list` for Method Dispatch
#'
#' `class_score_list` is an S7 subclass of S3 base R's `list`, used for method dispatch in
#' [bind_scores()] and [fill_safe_values()].
#'
#' @export
class_score_list <- S7::new_S3_class("list")

#' Bind all `class_score` objects, including their associated metadata and scores
#'
#' @param x A score class object.
#'
#' @param ... Further arguments passed to or from other methods.
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
#' # anova pval
#' ames_aov_pval_res <-
#'   score_aov_pval |>
#'   fit(Sale_Price ~ ., data = ames_subset)
#' ames_aov_pval_res@results
#'
#' # pearson cor
#' ames_cor_pearson_res <-
#'   score_cor_pearson |>
#'   fit(Sale_Price ~ ., data = ames_subset)
#' ames_cor_pearson_res@results
#'
#' # forest imp
#' score_imp_rf_reg <- score_imp_rf
#' score_imp_rf_reg@mode <- "regression"
#' set.seed(42)
#' ames_imp_rf_reg_res <-
#'   score_imp_rf_reg |>
#'   fit(Sale_Price ~ ., data = ames_subset)
#' ames_imp_rf_reg_res@results
#'
#' # info gain
#' score_info_gain_reg <- score_info_gain
#' score_info_gain_reg@mode <- "regression"
#'
#' ames_info_gain_reg_res <-
#'   score_info_gain_reg |>
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
#' @export
bind_scores <- S7::new_generic("bind_scores", dispatch_args = "x")

#' @noRd
#' @export
S7::method(bind_scores, class_score_list) <- function(x) {
  length_x <- length(x)
  if (length_x < 2) {
    cli::cli_abort(
      "{.arg x} must contain at least two elements"
    )
  } else {
    # TODO Check for identical score object, e.g., list(score_obj_aov, score_obj_aov)
    score_set <- x[[1]]@results
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
#' Fill safe values
#'
#' Wraps [bind_scores()] and fills in safe values for missing scores.
#'
#' @param x A score class object.
#'
#' @param ... Further arguments passed to or from other methods.
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
#' # anova pval
#' ames_aov_pval_res <-
#'   score_aov_pval |>
#'   fit(Sale_Price ~ ., data = ames_subset)
#' ames_aov_pval_res@results
#'
#' # pearson cor
#' ames_cor_pearson_res <-
#'   score_cor_pearson |>
#'   fit(Sale_Price ~ ., data = ames_subset)
#' ames_cor_pearson_res@results
#'
#' # forest imp
#' score_imp_rf_reg <- score_imp_rf
#' score_imp_rf_reg@mode <- "regression"
#' set.seed(42)
#' ames_imp_rf_reg_res <-
#'   score_imp_rf_reg |>
#'   fit(Sale_Price ~ ., data = ames_subset)
#' ames_imp_rf_reg_res@results
#'
#' # info gain
#' score_info_gain_reg <- score_info_gain
#' score_info_gain_reg@mode <- "regression"
#'
#' ames_info_gain_reg_res <-
#'   score_info_gain_reg |>
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
#' @export
fill_safe_values <- S7::new_generic("fill_safe_values", dispatch_args = "x")

#' @noRd
#' @export
S7::method(fill_safe_values, class_score_list) <- function(x) {
  # TODO Max was saying maybe we can fill safe value as we merge in (PR #33)
  score_set <- bind_scores(x)
  for (i in 1:length(x)) {
    method_name <- x[[i]]@score_type
    fallback_val <- x[[i]]@fallback_value
    is_na_score <- is.na(score_set[[method_name]])
    score_set[[method_name]][is_na_score] <- fallback_val
  }
  score_set
}

# # TODO Drop outcome column
# # TODO rank_desirability_score_*
# # TODO filter_desirability_score_*

# ------------------------------------------------------------------------------
# Used with ANOVA methods

flip_if_needed_aov <- function(predictor, outcome) {
  if (is.factor(outcome) && is.numeric(predictor)) {
    list(predictor = outcome, outcome = predictor)
  } else {
    list(predictor = predictor, outcome = outcome)
  }
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
