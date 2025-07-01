#' Attach score result `score_res` to score object `score_obj` containig metadata
#'
#' @param x NULL
#'
#' @param ... NULL
#'
#' @export
attach_score <- function(x, ...) {
  UseMethod("attach_score")
}

#' @noRd
#' @export
attach_score.default <- function(x, score_res, ...) {
  cli::cli_abort(
    "{.arg x} must be {.cls score_obj}, not {.obj_type_friendly {x}}."
  )
}

#' @noRd
#' @export
attach_score.score_obj <- function(x, score_res, ...) {
  x$score_res <- score_res
  x
}

#' Arrange score result `score_res`
#'
#' @param x NULL
#'
#' @param ... NULL
#'
#' @export
arrange_score <- function(x, ...) {
  UseMethod("arrange_score")
}

#' @noRd
#' @export
arrange_score.default <- function(x, ..., target = NULL) {
  cli::cli_abort(
    "{.arg x} must be {.cls score_obj}, not {.obj_type_friendly {x}}."
  )
}

#' @noRd
#' @export
arrange_score.score_obj <- function(x, ..., target = NULL) {
  # TODO Check if direction == target, add "need a target"
  if (x$direction == "maximize") {
    x$score_res |> dplyr::arrange(dplyr::desc(score))
  } else if (x$direction == "minimize") {
    x$score_res |> dplyr::arrange(score)
  } else if (x$direction == "target") {
    x$score_res |> dplyr::arrange(abs(score - target))
  }
}

#' Transform score result `score_res`
#'
#' @param x NULL
#'
#' @param ... NULL
#'
#' @export
trans_score <- function(x, ...) {
  UseMethod("trans_score")
}

#' @noRd
#' @export
trans_score.default <- function(x, ...) {
  cli::cli_abort(
    "{.arg x} must be {.cls score_obj}, not {.obj_type_friendly {x}}."
  )
}

#' @noRd
#' @export
trans_score.score_obj <- function(x, ...) {
  # TODO Figure out the basic structure first then come back for this. Have user supply direction =, if they use trans.
  if (is.null(x$trans)) {
    trans <- scales::transform_identity()
  } else {
    trans <- x$trans
  }
  x$score_res |>
    dplyr::mutate(score = trans$transform(score))
}

#' Filter score result `score_res` based on number of predictors
#'
#' @param x NULL
#'
#' @param ... NULL
#'
#' @export
filter_score_num <- function(x, ...) {
  UseMethod("filter_score_num")
}

#' @noRd
#' @export
filter_score_num.default <- function(x, ..., num_terms, target = NULL) {
  cli::cli_abort(
    "{.arg x} must be {.cls score_obj}, not {.obj_type_friendly {x}}."
  )
}

#' @noRd
#' @export
filter_score_num.score_obj <- function(x, ..., num_terms, target = NULL) {
  # TODO Handle ties here?
  # TODO Check if direction == target, add "need a target"
  if (rlang::is_missing(num_terms)) {
    cli::cli_abort("{.arg num_terms} must be specified")
  }

  if (x$direction == "maximize") {
    x$score_res |> dplyr::slice_max(score, n = num_terms)
  } else if (x$direction == "minimize") {
    x$score_res |> dplyr::slice_min(score, n = num_terms)
  } else if (x$direction == "target") {
    x$score_res |>
      dplyr::arrange(abs(score - target)) |>
      dplyr::slice_head(n = num_terms)
  }
}

#' Filter score result `score_res` based on proportion of predictors
#'
#' @param x NULL
#'
#' @param ... NULL
#'
#' @export
filter_score_prop <- function(x, ...) {
  UseMethod("filter_score_prop")
}

#' @noRd
#' @export
filter_score_prop.default <- function(x, ..., prop_terms, target = NULL) {
  cli::cli_abort(
    "{.arg x} must be {.cls score_obj}, not {.obj_type_friendly {x}}."
  )
}

#' @noRd
#' @export
filter_score_prop.score_obj <- function(x, ..., prop_terms, target = NULL) {
  # TODO Handle ties here?
  # TODO Check if direction == target, add "need a target"
  if (rlang::is_missing(prop_terms)) {
    cli::cli_abort("{.arg prop_terms} must be specified")
  }

  if (x$direction == "maximize") {
    x$score_res |> dplyr::slice_max(score, prop = prop_terms)
  } else if (x$direction == "minimize") {
    x$score_res |> dplyr::slice_min(score, prop = prop_terms)
  } else if (x$direction == "target") {
    x$score_res |>
      dplyr::arrange(abs(score - target)) |>
      dplyr::slice_head(prop = prop_terms)
  }
}

#' Filter score result `score_res` based on cutoff value
#'
#' @param x NULL
#'
#' @param ... NULL
#'
#' @export
filter_score_cutoff <- function(x, ...) {
  UseMethod("filter_score_cutoff")
}

#' @noRd
#' @export
filter_score_cutoff.default <- function(x, ..., cutoff, target = NULL) {
  cli::cli_abort(
    "{.arg x} must be {.cls score_obj}, not {.obj_type_friendly {x}}."
  )
}

#' @noRd
#' @export
filter_score_cutoff.score_obj <- function(x, ..., cutoff, target = NULL) {
  if (rlang::is_missing(cutoff)) {
    cli::cli_abort("{.arg cutoff} must be specified")
  }
  # TODO Check if direction == target, add "need a target"
  if (x$direction == "maximize") {
    # TODO Can return more # of predictors due to floating-point precision.
    x$score_res |>
      dplyr::arrange(dplyr::desc(score)) |>
      dplyr::filter(score >= cutoff)
  } else if (x$direction == "minimize") {
    # TODO Can return less # of predictors due to floating-point precision.
    x$score_res |> dplyr::arrange(score) |> dplyr::filter(score <= cutoff)
  } else if (x$direction == "target") {
    # TODO This cutoff value is based on abs(score - target). Not ideal?
    x$score_res |>
      dplyr::arrange(abs(score - target)) |>
      dplyr::filter(abs(score - target) <= cutoff)
  }
}

# TODO Filter score result `score_res` where user can
# based on number of predictors with option to use cutoff value
# OR proportion of predictors with option to use cutoff value.
# Mimic colino's `dual_filter`.

# TODO Filter score result `score_res` based on user input
# filter_score_<>

#' Rank score result `score_res` based on min_rank (Need a better title)
#'
#' @param x NULL
#'
#' @param ... NULL
#'
#' @export
rank_score_min <- function(x, ...) {
  UseMethod("rank_score_min")
}

#' @noRd
#' @export
rank_score_min.default <- function(x, ...) {
  cli::cli_abort(
    "{.arg x} must be {.cls score_obj}, not {.obj_type_friendly {x}}."
  )
}

#' @noRd
#' @export
rank_score_min.score_obj <- function(x, ..., target = NULL) {
  # TODO Check if direction == target, add "need a target"
  if (x$direction == "maximize") {
    x$score_res |> dplyr::mutate(rank = dplyr::min_rank((dplyr::desc(score))))
  } else if (x$direction == "minimize") {
    x$score_res |> dplyr::mutate(rank = dplyr::min_rank((score)))
  } # else if (x$direction == "target") { # TODO
  #   x$score_res |> dplyr::arrange(abs(score - target))
  # }
}

#' Rank score result `score_res` based on dense_rank (Need a better title)
#'
#' @param x NULL
#'
#' @param ... NULL
#'
#' @export
rank_score_dense <- function(x, ...) {
  UseMethod("rank_score_dense")
}

#' @noRd
#' @export
rank_score_dense.default <- function(x, ...) {
  cli::cli_abort(
    "{.arg x} must be {.cls score_obj}, not {.obj_type_friendly {x}}."
  )
}

#' @noRd
#' @export
rank_score_dense.score_obj <- function(x, ..., target = NULL) {
  # TODO Check if direction == target, add "need a target"
  if (x$direction == "maximize") {
    x$score_res |> dplyr::mutate(rank = dplyr::dense_rank((dplyr::desc(score))))
  } else if (x$direction == "minimize") {
    x$score_res |> dplyr::mutate(rank = dplyr::dense_rank((score)))
  } # else if (x$direction == "target") { # TODO
  #   x$score_res |> dplyr::arrange(abs(score - target))
  # }
}

# #' Assign class `result_obj` to score object `score_obj` TODO
# #'
# #' @param x NULL
# #'
# #' @param ... NULL
# #'
# #' @export
# as_result_obj <- function(x, ...) {
#   UseMethod("as_result_obj")
# }

# #' @noRd
# #' @export
# as_result_obj.score_obj <- function(x, res, ...) {
#   class(res) <- c("result_obj", class(res))
#   x$res <- res # COMMENT This overlaps with attach_score()
#   x
# }

#' Bind all metadata `score_obj` and score result `score_res`, and assign class `score_set` to combined scores.
#'
#' @param x A list where each element is a score object of class `score_obj`.
#'
#' @export
bind_scores <- function(x) {
  UseMethod("bind_scores")
}

#' @noRd
#' @export
bind_scores.default <- function(x) {
  cli::cli_abort(
    "{.arg x} must be {.cls list}, not {.obj_type_friendly {x}}."
  )
}

#' @noRd
#' @export
#'
#' @examples
#' # Create a list of `score_obj`
#' # Bind scores
#' score_obj_list <- list(score_obj_aov, score_obj_cor, score_obj_imp)
#' score_obj_list |> bind_scores()
#'
bind_scores.list <- function(x) {
  score_set <- x[[1]]$score_res
  for (i in 2:length(x)) {
    score_set <- dplyr::full_join(
      score_set,
      x[[i]]$score_res,
      by = c("name", "score", "outcome", "predictor") # OR suppressMessages()
    )
  }
  score_set <- score_set |>
    tidyr::pivot_wider(names_from = name, values_from = score)
  class(score_set) <- c("score_set", class(score_set))
  score_set
}

#' Fill in safe values.
#'
#' @param x A list where each element is a score object of class `score_obj`.
#'
#' @export
fill_safe_values <- function(x) {
  UseMethod("fill_safe_values")
}

#' @noRd
#' @export
fill_safe_values.default <- function(x) {
  cli::cli_abort(
    "{.arg x} must be {.cls list}, not {.obj_type_friendly {x}}."
  )
}

#' @noRd
#' @export
#'
#' @examples
#' # Create a list of `score_obj`
#' # Fill in safe values
#' score_obj_list <- list(score_obj_aov, score_obj_cor, score_obj_imp)
#' score_obj_list |> fill_safe_values()
#'
fill_safe_values.list <- function(x) {
  score_set <- x[[1]]$score_res
  for (i in 2:length(x)) {
    score_set <- dplyr::full_join(
      score_set,
      x[[i]]$score_res,
      by = c("name", "score", "outcome", "predictor") # OR suppressMessages()
    )
  }

  for (i in 1:length(x)) {
    # TODO Wonder if there is a cleaner way to do it. map?
    method_name <- unique(x[[i]]$score_res$name)
    fallback_val <- x[[i]]$fallback_value
    score_set <- score_set |>
      dplyr::mutate(
        score = ifelse(
          is.na(score) & name == method_name,
          fallback_val,
          score
        )
      )
  }

  score_set <- score_set |>
    tidyr::pivot_wider(names_from = name, values_from = score)

  class(score_set) <- c("score_set", class(score_set))
  score_set
}

# TODO Filter *

# TODO Drop outcome column
