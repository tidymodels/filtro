transform_abs <- scales::trans_new(
  # TODO Put an issue to scales. Point to this file.
  name = "abs",
  transform = function(x) abs(x),
  inverse = function(x) x # dummy inverse
)

transform_neg_log10 <- scales::trans_new(
  name = "neg-log-10",
  transform = function(x) -log10(x),
  inverse = function(x) 10^(-x)
)
