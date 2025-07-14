#' Attach score result `score_res` to score object `score_obj` containig metadata
#'
#' @param x NULL
#'
#' @param ... NULL
#'
#' @export
attach_score <- S7::new_generic(
  "attach_score",
  "x",
  function(x, score_res, ...) {
    if (!S7::S7_inherits(x, new_score_obj)) {
      cli::cli_abort(
        "{.arg x} must be a {.cls new_score_obj}, not {.obj_type_friendly {x}}."
      )
    }

    # if (!S7::S7_inherits(score_res, "data.frame")) {
    #   cli::cli_abort(
    #     "{.arg score_res} must be a tibble (a {.cls tbl_df}), not {.obj_type_friendly {score_res}}."
    #   )
    # }

    S7::S7_dispatch()
  }
)

#' @noRd
#' @export
S7::method(attach_score, new_score_obj) <- function(x, score_res, ...) {
  x@score_res <- score_res
  x
}

#' Arrange score result `score_res`
#'
#' @param x NULL
#'
#' @param ... NULL
#'
#' @export
arrange_score <- S7::new_generic(
  "arrange_score",
  "x",
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
    x@score_res |> dplyr::arrange(dplyr::desc(score))
  } else if (x@direction == "minimize") {
    x@score_res |> dplyr::arrange(score)
  } else if (x@direction == "target") {
    check_target(target)
    x@score_res |> dplyr::arrange(abs(score - target))
  }
}

#' Transform score result `score_res`
#'
#' @param x NULL
#'
#' @param ... NULL
#'
#' @export
trans_score <- S7::new_generic(
  "trans_score",
  "x",
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
  x@score_res |>
    dplyr::mutate(score = trans$transform(score))
}

#' Filter score result `score_res` based on number of predictors
#'
#' @param x NULL
#'
#' @param ... NULL
#'
#' @export
filter_score_num <- S7::new_generic(
  "filter_score_num",
  "x",
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
    x@score_res |> dplyr::slice_max(score, n = num_terms)
  } else if (x@direction == "minimize") {
    x@score_res |> dplyr::slice_min(score, n = num_terms)
  } else if (x@direction == "target") {
    check_target(target)
    x@score_res |>
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
filter_score_prop <- S7::new_generic(
  "filter_score_prop",
  "x",
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

  if (x@direction == "maximize") {
    x@score_res |> dplyr::slice_max(score, prop = prop_terms)
  } else if (x@direction == "minimize") {
    x@score_res |> dplyr::slice_min(score, prop = prop_terms)
  } else if (x@direction == "target") {
    check_target(target)
    x@score_res |>
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
  check_cutoff(cutoff)

  # TODO Can return more # of predictors due to floating-point precision
  if (x$direction == "maximize") {
    x$score_res |> dplyr::filter(score >= cutoff)
  } else if (x$direction == "minimize") {
    x$score_res |> dplyr::filter(score <= cutoff)
  } else if (x$direction == "target") {
    check_target(target)
    x$score_res |> dplyr::filter(abs(score - target) <= cutoff)
  }
}

#' Filter score result `score_res` based on number or proportion of predictors with
#' optional cutoff value
#'
#' @param x NULL
#'
#' @param ... NULL
#'
#' @export
filter_score_auto <- function(x, ...) {
  UseMethod("filter_score_auto")
}

#' @noRd
#' @export
filter_score_auto.default <- function(x, ...) {
  cli::cli_abort(
    "{.arg x} must be {.cls score_obj}, not {.obj_type_friendly {x}}."
  )
}

#' @noRd
#' @export
filter_score_auto.score_obj <- function(
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
    score_res <- filter_score_num(x, ..., num_terms = num_terms)
  } else if (!is.null(prop_terms)) {
    score_res <- filter_score_prop(
      x,
      ...,
      prop_terms = prop_terms
    )
  }
  if (!is.null(cutoff)) {
    score_res <- filter_score_cutoff(x, ..., cutoff = cutoff)
  }
  score_res
}

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
  length_x <- length(x)
  if (length_x < 2) {
    cli::cli_abort(
      "{.arg x} must contain at least two elements"
    )
  } else {
    # TODO Check for identical score object, e.g., list(score_obj_aov, score_obj_aov)
    score_set <- x[[1]]$score_res
    for (i in 2:length_x) {
      score_set <- dplyr::full_join(
        score_set,
        x[[i]]$score_res,
        by = c("name", "score", "outcome", "predictor")
      )
    }
  }

  score_set <- score_set |>
    tidyr::pivot_wider(names_from = name, values_from = score)

  class(score_set) <- c("score_set", class(score_set))
  score_set
}

#' Fill safe values.
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
  # TODO Max was saying maybe we can fill safe value as we merge in (PR #33)
  score_set <- bind_scores(x)
  for (i in 1:length(x)) {
    method_name <- x[[i]]$score_type
    fallback_val <- x[[i]]$fallback_value
    score_set[[method_name]][is.na(score_set[[method_name]])] <- fallback_val
  }
  score_set
}

# TODO Drop outcome column

# TODO filter_score_*
# Looking ahead at the example in desiarbility2, I think we'd want
# predictor, d_fstat, d_pval, d_pearson, d_imp_rf, d_roc_auc, etc, d_all.
