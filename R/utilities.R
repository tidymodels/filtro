#' Attach score to score object
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
attach_score.default <- function(x, res, ...) {
  cli::cli_abort(
    "{.arg x} must be {.cls score_obj}, not {.obj_type_friendly {x}}."
  )
}

#' @noRd
#' @export
attach_score.score_obj <- function(x, res, ...) {
  x$res <- res
  x
}

#' Arrange score
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
arrange_score.score_obj <- function(x, ..., target = NULL) {
  # TODO Check if direction == target, add "need a target"
  if (x$direction == "maximize") {
    x$res |> dplyr::arrange(dplyr::desc(score))
  } else if (x$direction == "minimize") {
    x$res |> dplyr::arrange(score)
  } else if (x$direction == "target") {
    x$res |> dplyr::arrange(abs(score - target))
  }
}

#' Transform score
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
trans_score.score_obj <- function(x, ...) {
  if (is.null(x$trans)) {
    trans <- scales::transform_identity()
  } else {
    trans <- x$trans
  }
  x$res |>
    dplyr::mutate(score = trans$transform(score))
}

#' Filter score based on number of predictors
#'
#' @param x NULL
#'
#' @param ... NULL
#'
#' @export
filter_score_num <- function(x, ...) {
  # TODO Rename to filter_num_terms?
  UseMethod("filter_score_num")
}

#' @noRd
#' @export
filter_score_num.score_obj <- function(x, ..., num_terms, target = NULL) {
  if (x$direction == "maximize") {
    x$res |> dplyr::slice_max(score, n = num_terms)
  } else if (x$direction == "minimize") {
    x$res |> dplyr::slice_min(score, n = num_terms)
  } else if (x$direction == "target") {
    x$res |>
      dplyr::arrange(abs(score - target)) |>
      dplyr::slice_head(n = num_terms)
  }
}

#' Filter score based on proportion of predictors
#'
#' @param x NULL
#'
#' @param ... NULL
#'
#' @export
filter_score_prop <- function(x, ...) {
  # TODO Rename to filter_num_terms?
  UseMethod("filter_score_prop")
}

#' @noRd
#' @export
filter_score_prop.score_obj <- function(x, ..., prop_terms, target = NULL) {
  if (x$direction == "maximize") {
    x$res |> dplyr::slice_max(score, prop = prop_terms)
  } else if (x$direction == "minimize") {
    x$res |> dplyr::slice_min(score, prop = prop_terms)
  } else if (x$direction == "target") {
    x$res |>
      dplyr::arrange(abs(score - target)) |>
      dplyr::slice_head(prop = prop_terms)
  }
}

# TODO Filter score based on cutoff value
# filter_score_cutoff

# TODO Rank score
# rank_score
