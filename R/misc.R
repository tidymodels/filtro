#' Absolute Transformation
#'
#' @export
transform_abs <- scales::trans_new(
  name = "abs",
  transform = function(x) abs(x),
  inverse = function(x) x # dummy inverse
)

#' Negative log10 Transformation (TODO Rewrite using other method)
#'
#' @export
transform_neg_log10 <- scales::trans_new(
  name = "neg-log-10",
  transform = function(x) -log10(x),
  inverse = function(x) 10^(-x)
)

#' @noRd
#' @keywords internal
check_target <- function(target) {
  if (is.null(target)) {
    cli::cli_abort(
      "{.arg target} must be supplied when {.arg direction} is {.val target}."
    )
  }
}

#' @noRd
#' @keywords internal
check_num_terms <- function(num_terms) {
  if (is.null(num_terms)) {
    cli::cli_abort("{.arg num_terms} must be specified")
  }
}

#' @noRd
#' @keywords internal
check_prop_terms <- function(prop_terms) {
  if (is.null(prop_terms)) {
    cli::cli_abort("{.arg prop_terms} must be specified")
  }
}

#' @noRd
#' @keywords internal
check_cutoff <- function(cutoff) {
  if (is.null(cutoff)) {
    cli::cli_abort("{.arg cutoff} must be specified")
  }
}

# TODO Check num_items & prop_items
