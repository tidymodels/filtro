#' Title
#'
#' @param subclass A character string.
#' @param outcome_type A character string. One of:
#'  - `"numeric"`
#'  - `"factor"`
#'
#' @param predictor_type A character string. One of:
#'  - `"numeric"`
#'  - `"factor"`
#'
#' @param case_weights NULL
#' @param range A numeric vector of length two, specifying the minimum and maximum
#' possible values, respectively.
#' @param inclusive A logical vector of length two, indicating whether the lower and
#' upper bounds of the range are inclusive (`TRUE`) or exclusive (`FALSE`), respectively.
#' @param fallback_value A numeric scalar used as a fallback value. Typical values
#' include: `0`, `1`, or `Inf`.
#' @param score_type A character string.
#' @param trans A `trans` object from the \pkg{scales} package, such as
#' [scales::transform_log10()] or [scales::transform_reciprocal()]. Or use built-in
#' functions, such as.
#' Create custom transforms with [scales::trans_new()].
#' @param sorts NULL
#' @param direction A character string. One of:
#'  - `"maximize"`
#'  - `"minimize"`
#'  - `"target"`
#'
#' @param deterministic A logical value, indicating whether the resulting score is
#' deterministic (`TRUE`) or random (`FALSE`).
#' @param tuning A logical value, indicating whether the model should be tuned
#' (`TRUE`) or not (`FALSE`).
#' @param ties A logical value indicating whether ties in score can occur (`TRUE`)
#' or not (`FALSE`).
#' @param calculating_fn NULL
#' @param label NULL
#' @param ... NULL
#'
#' @returns NULL
#' @export
#'
#' @examples NULL
new_score_obj <- function(
  subclass = c("cat_num", "cat_cat", "num_num", "any"),
  outcome_type = c("numeric", "factor"),
  predictor_type = c("numeric", "factor"),
  case_weights = NULL,
  range = NULL,
  inclusive = NULL,
  fallback_value = NULL,
  score_type = NULL,
  trans = NULL,
  sorts = NULL,
  direction = NULL,
  deterministic = NULL,
  tuning = NULL,
  ties = NULL,
  calculating_fn = NULL,
  label = NULL,
  ...
) {
  # TODO Include validators here
  # TODO Add a validator to make sure subclass has to be in num_num, cat_num, cat_cat, any

  res <- list(
    outcome_type = outcome_type,
    predictor_type = predictor_type,
    case_weights = case_weights,
    range = range,
    inclusive = inclusive,
    fallback_value = fallback_value,
    score_type = score_type,
    trans = trans,
    sorts = sorts,
    direction = direction,
    deterministic = deterministic,
    tuning = tuning,
    ties = ties,
    calculating_fn = calculating_fn,
    label = label
  )
  class(res) <- c(subclass, "score_obj") # TODO Rename subclass

  res
}
