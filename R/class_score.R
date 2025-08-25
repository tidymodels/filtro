#' General S7 classes for scoring objects
#'
#' `class_score` is an S7 object that contains slots for the characteristics of
#' predictor importance scores. More specific classes for individual methods are
#' based on this object (shown below).
#' @keywords internal
#' @export
class_score <- S7::new_class(
  "class_score",
  properties = list(
    # What types of outcome can the method handle?
    outcome_type = S7::new_property(
      S7::class_character,
      default = c("numeric", "factor")
    ),
    # What types of predictor can the method handle?
    predictor_type = S7::new_property(
      S7::class_character,
      default = c("numeric", "factor")
    ),
    # Does the method accept case weights?
    case_weights = S7::class_logical,
    # Are there known ranges for the statistic?
    range = S7::class_numeric,
    # Are these ranges inclusive at the bounds?
    inclusive = S7::class_logical,
    # What is a value that can be used for the statistic so that it will never
    # be eliminated?
    fallback_value = S7::class_numeric,
    # What is the column name that will be used for the statistic values?
    score_type = S7::class_character,
    # How should the values be transformed?
    transform_fn = S7::class_function,
    # What direction of values indicates the most important values?
    direction = S7::class_character,
    # Does the fitting process use random numbers?
    deterministic = S7::class_logical,
    # Does the method have tuning parameters?
    tuning = S7::class_logical,
    # What function, if any, is used to estimate the values from data?
    calculating_fn = S7::class_function,
    # What label to use when printing?
    label = S7::class_character,
    # What packages, if any, are required to train the method?
    packages = S7::class_character,
    # A slot for the results once the method is fitted.
    results = S7::new_property(
      S7::class_data.frame,
      default = quote(data.frame())
    )
  )
)

# ------------------------------------------------------------------------------
# Methods

#' @keywords internal
#' @export
S7::method(required_pkgs, class_score) <- function(x, ...) {
  # Always include the parent package along with the method's dependencies
  sort(unique(c("filtro", x@packages)))
}
