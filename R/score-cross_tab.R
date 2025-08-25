#' @rdname class_score
#' @include class_score.R
#' @keywords internal
#' @export
class_score_xtab <- S7::new_class(
  "class_score_xtab",
  parent = class_score,
  properties = list(
    # Represent the score as -log10(p_value)?
    neg_log10 = S7::new_property(S7::class_logical, default = TRUE)
  )
)

#' Scoring via the chi-squared test or Fisher's exact test
#'
#' @description
#'
#' These two objects can be used to compute importance scores based on
#' chi-squared test or Fisher's exact test.
#'
#' @name score_xtab_pval_chisq
#' @family class score metrics
#'
#' @details
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
#' ## Estimating the scores
#'
#' In \pkg{filtro}, the `score_*` objects define a scoring method (e.g., data
#' input requirements, package dependencies, etc). To compute the scores for
#' a specific data set, the `fit()` method is used. The main arguments for
#' these functions are:
#'
#'   \describe{
#'     \item{`object`}{A score class object (e.g., `score_xtab_pval_chisq`).}
#'     \item{`formula`}{A standard R formula with a single outcome on the right-hand side and one or more predictors (or `.`) on the left-hand side. The data are processed via [stats::model.frame()]}
#'     \item{`data`}{A data frame containing the relevant columns defined by the formula.}
#'     \item{`...`}{Further arguments passed to or from other methods.}
#'     \item{`case_weights`}{A quantitative vector of case weights that is the same length as the number of rows in `data`. The default of `NULL` indicates that there are no case weights.}
#'   }
#'
#' @includeRmd man/rmd/missing_delete.Rmd details
#'
#' @includeRmd man/rmd/fault_tolerant.Rmd details
#'
#' @return An S7 object. The primary property of interest is in `results`. This
#' is a data frame of results that is populated by the `fit()` method and has
#' columns:
#'
#' - `name`: The name of the score (e.g., `pval_chisq`).
#' - `score`: The estimates for each predictor.
#' - `outcome`: The name of the outcome column.
#' - `predictor`: The names of the predictor inputs.
#'
#' These data are accessed using `object@results` (see examples below).
#'
#' @examplesIf rlang::is_installed(c("titanic", "modeldata"))
#' # Binary factor example
#'
#' library(titanic)
#' library(dplyr)
#'
#' titanic_subset <- titanic_train |>
#'   mutate(across(c(Survived, Pclass, Sex, Embarked), as.factor)) |>
#'   select(Survived, Pclass, Sex, Age, Fare, Embarked)
#'
#' # Chi-squared test
#' titanic_xtab_pval_chisq_res <- score_xtab_pval_chisq |>
#'   fit(Survived ~ ., data = titanic_subset)
#' titanic_xtab_pval_chisq_res@results
#'
#' # Chi-squared test adjusted p-values
#' titanic_xtab_pval_chisq_p_adj_res <- score_xtab_pval_chisq |>
#'   fit(Survived ~ ., data = titanic_subset, adjustment = "BH")
#'
#' # Fisher's exact test
#' titanic_xtab_pval_fisher_res <- score_xtab_pval_fisher |>
#'   fit(Survived ~ ., data = titanic_subset)
#' titanic_xtab_pval_fisher_res@results
#'
#' # Chi-squared test where `class` is the multiclass outcome/response
#'
#' hpc_subset <- modeldata::hpc_data |>
#'   dplyr::select(
#'     class,
#'     protocol,
#'     hour
#'   )
#'
#' hpc_xtab_pval_chisq_res <- score_xtab_pval_chisq |>
#'     fit(class ~ ., data = hpc_subset)
#' hpc_xtab_pval_chisq_res@results
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
    transform_fn = function(x) x,
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
    transform_fn = function(x) x,
    direction = "maximize",
    deterministic = TRUE,
    tuning = FALSE,
    label = "Fisher's exact test cross tabulation p-values"
  )

# ------------------------------------------------------------------------------

#' @export
S7::method(fit, class_score_xtab) <- function(
  object,
  formula,
  data,
  adjustment = "none",
  ...
) {
  analysis_data <- process_all_data(formula, data = data)
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

  if (length(adjustment) > 0 && adjustment != "none") {
    res$score <- stats::p.adjust(res$score, method = adjustment)
  }

  if (object@neg_log10) {
    res$score <- -log10(res$score)
  } else {
    object@range <- c(0.0, 1.0)
    object@inclusive <- rep(TRUE, 2)
    object@fallback_value <- .Machine$double.neg.eps
    object@direction <- "minimize"
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
    res <- try(suppressWarnings(stats::chisq.test(tab)$p.value), silent = TRUE)
    if (inherits(res, "try-error")) {
      res <- NA_real_
    }
  } else {
    tab <- table(predictor, sample(outcome))
    res <- try(suppressWarnings(stats::chisq.test(tab)$p.value), silent = TRUE)
    if (inherits(res, "try-error")) {
      res <- NA_real_
    }
  }
  return(res)
}

get_single_fisher <- function(predictor, outcome) {
  tab <- table(predictor, outcome)
  res <- try(suppressWarnings(stats::fisher.test(tab)$p.value), silent = TRUE)
  if (inherits(res, "try-error")) {
    res <- NA_real_
  }
  return(res)
}
