#' @param score_obj NULL
#'
#' @param ... NULL
#'
#' @export
attach_score <- function(score_obj, ...) {
  UseMethod("attach_score")
}

#' @noRd
#' @export
attach_score.any <- function(score_obj, res, ...) {
  score_obj$res <- res
  score_obj
}

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
arrange_score.any <- function(score_obj, target = target, ...) {
  if (score_obj$direction == "maximize") {
    score_obj$res |> dplyr::arrange(desc(score))
  } else if (score_obj$direction == "minimize") {
    score_obj$res |> dplyr::arrange(score)
  } else if (score_obj$direction == "target") {
    score_obj$res |> dplyr::arrange(abs(score - target))
  }
}
