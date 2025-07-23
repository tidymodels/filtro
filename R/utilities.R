# ------------------------------------------------------------------------------
#' Arrange score result `results`
#'
#' @param x NULL
#'
#' @param ... NULL
#'
#' @export
arrange_score <- S7::new_generic(
  "arrange_score",
  dispatch_args = "x",
  function(x, ...) {
    if (!S7::S7_inherits(x, new_score_obj)) {
      cli::cli_abort(
        "{.arg x} must be a {.cls new_score_obj}, not {.obj_type_friendly {x}}."
      )
    }

    S7::S7_dispatch()
  }
)

#' @noRd
#' @export
S7::method(arrange_score, new_score_obj) <- function(x, ..., target = NULL) {
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
#' Transform score result `results`
#'
#' @param x NULL
#'
#' @param ... NULL
#'
#' @export
trans_score <- S7::new_generic(
  "trans_score",
  dispatch_args = "x",
  function(x, ...) {
    if (!S7::S7_inherits(x, new_score_obj)) {
      cli::cli_abort(
        "{.arg x} must be a {.cls new_score_obj}, not {.obj_type_friendly {x}}."
      )
    }

    S7::S7_dispatch()
  }
)

#' @noRd
#' @export
S7::method(trans_score, new_score_obj) <- function(x, ...) {
  # TODO Figure out the basic structure first then come back for this. Have user supply direction =, if they use trans.
  if (is.null(x@trans)) {
    trans <- scales::transform_identity()
  } else {
    trans <- x@trans
  }
  x@results |>
    dplyr::mutate(score = trans$transform(score))
}

# ------------------------------------------------------------------------------
#' Filter score result `results` based on number of predictors
#'
#' @param x NULL
#'
#' @param ... NULL
#'
#' @export
filter_score_num <- S7::new_generic(
  "filter_score_num",
  dispatch_args = "x",
  function(x, ...) {
    if (!S7::S7_inherits(x, new_score_obj)) {
      cli::cli_abort(
        "{.arg x} must be a {.cls new_score_obj}, not {.obj_type_friendly {x}}."
      )
    }

    S7::S7_dispatch()
  }
)

#' @noRd
#' @export
S7::method(filter_score_num, new_score_obj) <- function(
  x,
  ...,
  num_terms,
  target = NULL
) {
  # TODO Handle ties here?
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
#' Filter score result `results` based on proportion of predictors
#'
#' @param x NULL
#'
#' @param ... NULL
#'
#' @export
filter_score_prop <- S7::new_generic(
  "filter_score_prop",
  dispatch_args = "x",
  function(x, ...) {
    if (!S7::S7_inherits(x, new_score_obj)) {
      cli::cli_abort(
        "{.arg x} must be a {.cls new_score_obj}, not {.obj_type_friendly {x}}."
      )
    }

    S7::S7_dispatch()
  }
)

#' @noRd
#' @export
S7::method(filter_score_prop, new_score_obj) <- function(
  x,
  ...,
  prop_terms,
  target = NULL
) {
  # TODO Handle ties here?
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
#' Filter score result `results` based on cutoff value
#'
#' @param x NULL
#'
#' @param ... NULL
#'
#' @export
filter_score_cutoff <- S7::new_generic(
  "filter_score_cutoff",
  dispatch_args = "x",
  function(x, ...) {
    if (!S7::S7_inherits(x, new_score_obj)) {
      cli::cli_abort(
        "{.arg x} must be a {.cls new_score_obj}, not {.obj_type_friendly {x}}."
      )
    }

    S7::S7_dispatch()
  }
)

#' @noRd
#' @export
S7::method(filter_score_cutoff, new_score_obj) <- function(
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
#' Filter score result `results` based on number or proportion of predictors with
#' optional cutoff value
#'
#' @param x NULL
#'
#' @param ... NULL
#'
#' @export
filter_score_auto <- S7::new_generic(
  "filter_score_auto",
  dispatch_args = "x",
  function(x, ...) {
    if (!S7::S7_inherits(x, new_score_obj)) {
      cli::cli_abort(
        "{.arg x} must be a {.cls new_score_obj}, not {.obj_type_friendly {x}}."
      )
    }

    S7::S7_dispatch()
  }
)

#' @noRd
#' @export
S7::method(filter_score_auto, new_score_obj) <- function(
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
    results <- filter_score_num(x, ..., num_terms = num_terms)
  } else if (!is.null(prop_terms)) {
    results <- filter_score_prop(
      x,
      ...,
      prop_terms = prop_terms
    )
  }
  if (!is.null(cutoff)) {
    results <- filter_score_cutoff(x, ..., cutoff = cutoff)
  }
  results
}

# Filter score result `results` based on user input TODO
# filter_score_<>

# ------------------------------------------------------------------------------
# #' Rank score result `results` based on min_rank (Need a better title) TODO
# #'
# #' @param x NULL
# #'
# #' @param ... NULL
# #'
# #' @export
# rank_score_min <- function(x, ...) {
#   UseMethod("rank_score_min")
# }

# #' @noRd
# #' @export
# rank_score_min.default <- function(x, ...) {
#   cli::cli_abort(
#     "{.arg x} must be {.cls score_obj}, not {.obj_type_friendly {x}}."
#   )
# }

# #' @noRd
# #' @export
# rank_score_min.score_obj <- function(x, ..., target = NULL) {
#   # TODO Check if direction == target, add "need a target"
#   if (x$direction == "maximize") {
#     x$results |> dplyr::mutate(rank = dplyr::min_rank((dplyr::desc(score))))
#   } else if (x$direction == "minimize") {
#     x$results |> dplyr::mutate(rank = dplyr::min_rank((score)))
#   } # else if (x$direction == "target") { # TODO
#   #   x$results |> dplyr::arrange(abs(score - target))
#   # }
# }

# ------------------------------------------------------------------------------
# #' Rank score result `results` based on dense_rank (Need a better title)
# #'
# #' @param x NULL
# #'
# #' @param ... NULL
# #'
# #' @export
# rank_score_dense <- function(x, ...) {
#   UseMethod("rank_score_dense")
# }

# #' @noRd
# #' @export
# rank_score_dense.default <- function(x, ...) {
#   cli::cli_abort(
#     "{.arg x} must be {.cls score_obj}, not {.obj_type_friendly {x}}."
#   )
# }

# #' @noRd
# #' @export
# rank_score_dense.score_obj <- function(x, ..., target = NULL) {
#   # TODO Check if direction == target, add "need a target"
#   if (x$direction == "maximize") {
#     x$results |> dplyr::mutate(rank = dplyr::dense_rank((dplyr::desc(score))))
#   } else if (x$direction == "minimize") {
#     x$results |> dplyr::mutate(rank = dplyr::dense_rank((score)))
#   } # else if (x$direction == "target") { # TODO
#   #   x$results |> dplyr::arrange(abs(score - target))
#   # }
# }

# ------------------------------------------------------------------------------
#' Construct an S7 subclass of base R's `list`
#'
#' Output an S7 subclass of S3 base R's `list`, used in method dispatch for
#' [bind_scores()] and [fill_safe_values()].
#'
#' @export
score_list <- S7::new_S3_class("list")

#' Bind all metadata `score_obj` and score result `results`.
#'
#' @param x A list where each element is a score object of class `score_obj`.
#' @param ... Further arguments passed to or from other methods.
#'
#' @export
bind_scores <- S7::new_generic("bind_scores", dispatch_args = "x")

#' @noRd
#' @export
#'
#' @examples
#' # Create a list of `score_obj`
#' # Bind scores
#' score_obj_list <- list(score_obj_aov, score_obj_cor, score_obj_imp)
#' score_obj_list |> bind_scores()
S7::method(bind_scores, score_list) <- function(x) {
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

  #class(score_set) <- c("score_set", class(score_set)) TODO Check with desirability2
  score_set
}

# ------------------------------------------------------------------------------
#' Fill safe values.
#'
#' @param x A list where each element is a score object of class `score_obj`.
#' @param ... Further arguments passed to or from other methods.
#'
#' @export
fill_safe_values <- S7::new_generic("fill_safe_values", dispatch_args = "x")

#' @noRd
#' @export
#'
#' @examples
#' # Create a list of `score_obj`
#' # Fill in safe values
#' score_obj_list <- list(score_obj_aov, score_obj_cor, score_obj_imp)
#' score_obj_list |> fill_safe_values()
S7::method(fill_safe_values, score_list) <- function(x) {
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

# TODO Drop outcome column
# TODO show_best_desirability_score_*
# TODO rank_desirability_score_*
# TODO filter_desirability_score_*

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
