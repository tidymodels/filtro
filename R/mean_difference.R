#' Calculate mean difference
#'
#' @param x A numeric vector.
#' @param y A numeric vector.
#'
#' @returns A single numeric value.
#' @export
#'
#' @examples
#' x <- c(1, 2, 3)
#' y <- c(4, 5, 6)
#' mean_difference(x, y)
mean_difference <- function(x, y) {
  abs(mean(x) - mean(y))
}
