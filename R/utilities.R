#' Attach score to filter object
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

#' Arrange score to filter object
#'
#' @param score_obj NULL
#'
#' @param ... NULL
#'
#' @export
arrange_score <- function(score_obj, ...) {
  UseMethod("arrange_score")
}

#' @noRd
#' @export
arrange_score.score_obj <- function(score_obj, ..., target = NULL) {
  # TODO Check if direction == target, add "need a target"
  if (score_obj$direction == "maximize") {
    score_obj$res |> dplyr::arrange(dplyr::desc(score))
  } else if (score_obj$direction == "minimize") {
    score_obj$res |> dplyr::arrange(score)
  } else if (score_obj$direction == "target") {
    score_obj$res |> dplyr::arrange(abs(score - target))
  }
}
