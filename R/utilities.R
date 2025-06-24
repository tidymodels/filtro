#' @param score_obj NULL
#'
#' @param ... NULL
#'
#' @export
#' @name attach_score
attach_score <- function(score_obj, ...) {
  UseMethod("attach_score")
}

#' @rdname attach_score
#' @method attach_score any
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
#' @name arrange_score
arrange_score <- function(score_obj, ...) {
  UseMethod("arrange_score")
}

#' @rdname arrange_score
#' @method arrange_score any
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
