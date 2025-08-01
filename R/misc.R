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
