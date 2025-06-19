# For creating new transformation object
transform_abs <- scales::trans_new(
  name = "abs",
  transform = function(x) abs(x),
  inverse = function(x) x # dummy inverse
)
transform_abs$transform(-1)
transform_abs$transform(c(-1, 1, 1))

transform_neg_log10 <- scales::trans_new(
  name = "neg-log-10",
  transform = function(x) -log10(x),
  inverse = function(x) 10^(-x)
)
transform_neg_log10$transform(0.01)
transform_neg_log10$transform(c(0.001, 0.01, 0.1))
