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

# TODO Check num_items

# TODO Check prop_items

# TODO Check cutoff

# TODO Check num_items & prop_items
