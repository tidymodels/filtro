#' @rdname class_score
#' @include class_score.R
#' @keywords internal
#' @export
class_score_xtab <- S7::new_class(
  "class_score_xtab",
  parent = class_score,
  properties = list(
    # Represent the score as -log10(p_value)?
    neg_log10 = S7::new_property(S7::class_logical, default = TRUE),
    # Control for the false discovery rate?
    fdr = S7::new_property(S7::class_logical, default = FALSE)
  )
)

#' Scoring via the chi-squared test or Fisher's exact test
#'
#' @description
#'
#' These objects are used when:
#'
#' - The predictors are factors and the outcome is a factor.
#'
#' In this case, a contingency table (via [table()]) is created with the proper
#' variable roles, and the cross tabulation p-value is computed using either
#' the chi-squared test (via [stats::chisq.test()]) or Fisher's exact test
#' (via [stats::fisher.test()]). The p-value that is returned is transformed to
#' be `-log10(p_value)` so that larger values are associated with more important
#' predictors.
#'
#' `score_xtab_pval_chisq` and `score_xtab_pval_fisher` are objects that define the technique.
#' To apply the filter on data, you would use the [fit()] method:
#'
#' \preformatted{
#'   fit(score_xtab_pval_chisq, formula, data)
#' }
#'
#' See the Examples section below.
#' @name score_xtab_pval_chisq
#' @export
score_xtab_pval_chisq <-
  class_score_xtab(
    outcome_type = "factor",
    predictor_type = "factor",
    case_weights = TRUE,
    range = c(0, Inf),
    inclusive = c(FALSE, FALSE),
    fallback_value = Inf,
    score_type = "xtab_pval_chisq",
    direction = "maximize",
    deterministic = TRUE,
    tuning = FALSE,
    label = "Chi-squared test cross tabulation p-values"
  )

#' @name score_xtab_pval_chisq
#' @export
score_xtab_pval_fisher <-
  class_score_xtab(
    outcome_type = "factor",
    predictor_type = "factor",
    case_weights = TRUE,
    range = c(0, Inf),
    inclusive = c(FALSE, FALSE),
    fallback_value = Inf,
    score_type = "xtab_pval_fisher",
    direction = "maximize",
    deterministic = TRUE,
    tuning = FALSE,
    label = "Fisher's exact test cross tabulation p-values"
  )

# ------------------------------------------------------------------------------

#' Compute cross tabulation p-value scores
#' @name score_xtab_pval_chisq
#' @include class_score.R
#' @param object A score class object based on `class_score_xtab`.
#' @param formula A standard R formula with a single outcome on the right-hand
#' side and one or more predictors (or `.`) on the left-hand side. The data are
#' processed via [stats::model.frame()].
#' @param data A data frame containing the relevant columns defined by the
#' formula.
#' @param ... Further arguments passed to or from other methods.
#' @details
#' The function will determine which columns are predictors and outcomes in the
#' contingency table; no user intervention is required.
#'
#' Missing values are removed for each predictor/outcome combination being
#' scored.
#' In cases where [stats::chisq.test()] or [stats::fisher.test()] fail, the scoring proceeds
#' silently, and a missing value is given for the score.
#'
#' @examplesIf rlang::is_installed("titanic")
#'
#' # Binary factor example
#'
#' library(titanic)
#' library(dplyr)
#'
#' titanic_subset <- titanic_train |>
#'   mutate(across(c(Survived, Pclass, Sex, Embarked), as.factor)) |>
#'   select(Survived, Pclass, Sex, Age, Fare, Embarked)
#'
#' titanic_xtab_pval_chisq_res <- score_xtab_pval_chisq |>
#'   fit(Survived ~ ., data = titanic_subset)
#' titanic_xtab_pval_chisq_res@results
#'
#' titanic_xtab_pval_fisher_res <- score_xtab_pval_fisher |>
#'   fit(Survived ~ ., data = titanic_subset)
#' titanic_xtab_pval_fisher_res@results
#' # TODO Add multiclass example
#' @export
S7::method(fit, class_score_xtab) <- function(object, formula, data, ...) {
  analysis_data <- process_all_data(formula, data = data)
  # TODO add case weights

  # Note that model.frame() places the outcome(s) as the first column(s)
  predictors <- names(analysis_data)[-1]
  outcome <- names(analysis_data)[1]

  if (object@score_type == "xtab_pval_chisq") {
    object@calculating_fn <- get_single_chisq
  } else if (object@score_type == "xtab_pval_fisher") {
    object@calculating_fn <- get_single_fisher
  }
  score <- purrr::map_dbl(
    purrr::set_names(predictors),
    \(x) {
      map_score_cross_tab(
        data,
        predictor = x,
        outcome = outcome,
        calculating_fn = object@calculating_fn
      )
    }
  )
  res <- named_vec_to_tibble(score, object@score_type, outcome)

  if (object@neg_log10) {
    res$score <- -log10(res$score)
  } else {
    object@range <- c(0.0, 1.0)
    object@inclusive <- rep(TRUE, 2)
    object@fallback_value <- .Machine$double.neg.eps
    object@sorts <- function(x) x
    object@direction <- "minimize"
  }

  if (object@fdr) {
    res$score <- stats::p.adjust(res$score, method = "BH")
  }

  object@results <- res
  object
}

map_score_cross_tab <- function(data, predictor, outcome, calculating_fn) {
  predictor_col <- data[[predictor]]
  outcome_col <- data[[outcome]]

  if (is.numeric(outcome_col) || is.numeric(predictor_col)) {
    return(NA_real_)
  }

  res <- calculating_fn(predictor_col, outcome_col)
  res
}

get_single_chisq <- function(predictor, outcome) {
  if (length(levels(outcome)) == 2) {
    tab <- table(predictor, outcome)
    res <- suppressWarnings(stats::chisq.test(tab)$p.value)
  } else {
    tab <- table(predictor, sample(outcome))
    res <- suppressWarnings(stats::chisq.test(tab)$p.value)
  }
  return(res)
}

get_single_fisher <- function(predictor, outcome) {
  tab <- table(predictor, outcome)
  res <- suppressWarnings(stats::fisher.test(tab)$p.value)
  return(res)
}
