#' @rdname class_score
#' @include class_score.R
#' @keywords internal
#' @export
class_score_aov <- S7::new_class(
  "class_score_aov",
  parent = class_score,
  properties = list(
    # Represent the score as -log10(p_value)?
    neg_log10 = S7::new_property(S7::class_logical, default = TRUE)
  )
)

#' Scoring via analysis of variance hypothesis tests
#'
#' @description
#'
#' These objects are used when either:
#'
#' - The predictors are numeric and the outcome is a factor/category, or
#' - The predictors are factors and the outcome is numeric.
#'
#' In either case, a linear model (vi [stats::lm()]) is created with the proper
#' variable roles, and the overall p-value for the hypothesis that all means are
#' equal is computed via the standard F-statistic. The p-value that is returned
#' is transformed to be `-log10(p_value)` so that larger values are associated
#' with more important predictors.
#'
#' `score_aov_pval` and `score_aov_fstat` are objects that define the technique.
#' To apply the filter on data, you would use the [fit()] method:
#'
#' \preformatted{
#'   fit(score_aov_pval, formula, data)
#' }
#'
#' See the Examples section below.
#' @name score_aov_pval
#' @export
score_aov_pval <-
  class_score_aov(
    outcome_type = c("numeric", "factor"),
    predictor_type = c("numeric", "factor"),
    case_weights = TRUE,
    range = c(0, Inf),
    inclusive = c(FALSE, FALSE),
    fallback_value = Inf,
    score_type = "aov_pval",
    direction = "maximize",
    deterministic = TRUE,
    tuning = FALSE,
    label = "ANOVA p-values"
  )

#' @name score_aov_pval
#' @export
score_aov_fstat <-
  class_score_aov(
    outcome_type = c("numeric", "factor"),
    predictor_type = c("numeric", "factor"),
    case_weights = TRUE,
    range = c(0, Inf),
    inclusive = c(FALSE, FALSE),
    fallback_value = Inf,
    score_type = "aov_fstat",
    direction = "maximize",
    deterministic = TRUE,
    tuning = FALSE,
    label = "ANOVA F-statistics"
  )

# ------------------------------------------------------------------------------

#' Compute analysis of variance scores
#' @name score_aov_pval
#' @include class_score.R
#' @param object A score class object based on `class_score_aov`.
#' @param formula A standard R formula with a single outcome on the right-hand
#' side and one or more predictors (or `.`) on the left-hand side. The data are
#' processed via [stats::model.frame()].
#' @param data A data frame containing the relevant columns defined by the
#' formula.
#' @param ... Further arguments passed to or from other methods.
#' @details
#' The function will determine which columns are predictors and outcomes in the
#' linear model; no user intervention is required.
#'
#' Missing values are removed for each predictor/outcome combination being
#' scored.
#' In cases where [lm()] or [anova()] fail, the scoring proceeds silently, and
#' a missing value is given for the score.
#'
#' @examples
#' if (rlang::is_installed("modeldata")) {
#'
#'   # Analysis of variance where `class` is the outcome and the numeric
#'   # predictors are the outcomes
#'
#'   cell_data <- modeldata::cells
#'   cell_data$case <- NULL
#'
#'   cell_p_val_res <-
#'     score_aov_pval |>
#'     fit(class ~ ., data = cell_data)
#'   cell_p_val_res@results
#'
#'   cell_t_stat_res <-
#'     score_aov_fstat |>
#'     fit(class ~ ., data = cell_data)
#'   cell_t_stat_res@results
#'
#'   # ----------------------------------------------------------------------------
#'   library(dplyr)
#'
#'   # Analysis of variance where `class` is the predictor data and the response
#'   # is the numeric outcome data
#'
#'   permeability <-
#'     modeldata::permeability_qsar |>
#'     # Make the problem a little smaller for time; use 50 predictors
#'     select(1:51) |>
#'     # Make the binary predictor columns into factors
#'     mutate(across(starts_with("chem_fp"), as.factor))
#'
#'   perm_p_val_res <-
#'     score_aov_pval |>
#'     fit(permeability ~ ., data = permeability)
#'   perm_p_val_res@results
#'
#'   # Note that some `lm()` calls failed and are given NA score values. For
#'   # example:
#'   table(permeability$chem_fp_0007)
#'
#'   perm_t_stat_res <-
#'     score_aov_fstat |>
#'     fit(permeability ~ ., data = permeability)
#'   perm_t_stat_res@results
#' }
#' @export
S7::method(fit, class_score_aov) <- function(object, formula, data, ...) {
  analysis_data <- process_all_data(formula, data = data)
  # TODO add case weights

  # Note that model.frame() places the outcome(s) as the first column(s)
  predictors <- names(analysis_data)[-1]
  outcome <- names(analysis_data)[1]

  use_pval <- object@score_type == "aov_pval"
  score <- purrr::map_dbl(
    purrr::set_names(predictors),
    \(x) {
      map_score_aov(
        analysis_data,
        predictor = x,
        outcome = outcome,
        pval = use_pval
      )
    }
  )
  res <- named_vec_to_tibble(score, object@score_type, outcome)

  if (use_pval) {
    if (object@neg_log10) {
      res$score <- -log10(res$score)
    } else {
      object@range <- c(0.0, 1.0)
      object@inclusive <- rep(TRUE, 2)
      object@fallback_value <- .Machine$double.neg.eps
      object@sorts <- function(x) x
      object@direction <- "minimize"
    }
  }

  object@results <- res
  object
}

map_score_aov <- function(data, predictor, outcome, pval) {
  predictor_col <- data[[predictor]]
  outcome_col <- data[[outcome]]

  # TODO Add general logic for these checks in process_all_data() and use when
  # that function is first called
  if (is.factor(outcome_col) && !is.numeric(predictor_col)) {
    return(NA_real_)
  }

  if (is.numeric(outcome_col) && !is.factor(predictor_col)) {
    return(NA_real_)
  }

  res <- get_single_aov(predictor_col, outcome_col, pval)
  res
}

get_single_aov <- function(predictor, outcome, pval = TRUE) {
  flipped <- flip_if_needed_aov(predictor, outcome)
  outcome <- flipped$outcome
  predictor <- flipped$predictor
  df <- tibble::tibble(outcome = outcome, predictor = predictor)

  fit <- try(
    stats::lm(outcome ~ predictor, data = df, na.action = "na.omit"),
    silent = TRUE
  )
  if (inherits(fit, "try-error")) {
    res <- NA_real_
  } else {
    res <- try(stats::anova(fit), silent = TRUE)
    if (inherits(res, "try-error")) {
      res <- NA_real_
    } else {
      if (pval) {
        res <- res$`Pr(>F)`[1]
      } else {
        res <- res$`F value`[1]
      }
    }
  }

  return(res)
}

