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
#' These two objects can be used to compute importance scores based on Analysis
#' of Variance techniques.
#'
#' @name score_aov_pval
#' @family class score metrics
#'
#' @details
#'
#' These objects are used when either:
#'
#' - The predictors are numeric and the outcome is a factor/category, or
#' - The predictors are factors and the outcome is numeric.
#'
#' In either case, a linear model (via [stats::lm()]) is created with the proper
#' variable roles, and the overall p-value for the hypothesis that all means are
#' equal is computed via the standard F-statistic. The p-value that is returned
#' is transformed to be `-log10(p_value)` so that larger values are associated
#' with more important predictors.
#'
#' ## Estimating the scores
#'
#' In \pkg{filtro}, the `score_*` objects define a scoring method (e.g., data
#' input requirements, package dependencies, etc). To compute the scores for
#' a specific data set, the `fit()` method is used. The main arguments for
#' these functions are:
#'
#'   \describe{
#'     \item{`object`}{A score class object (e.g., `score_aov_pval`).}
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
#' - `name`: The name of the score (e.g., `aov_fstat` or `aov_pval`).
#' - `score`: The estimates for each predictor.
#' - `outcome`: The name of the outcome column.
#' - `predictor`: The names of the predictor inputs.
#'
#' These data are accessed using `object@results` (see examples below).
#'
#' @examplesIf rlang::is_installed("modeldata")
#' # Analysis of variance where `class` is the class predictor and the numeric
#' # predictors are the outcomes/responses
#'
#' cell_data <- modeldata::cells
#' cell_data$case <- NULL
#'
#' # ANOVA p-value
#' cell_p_val_res <-
#'   score_aov_pval |>
#'   fit(class ~ ., data = cell_data)
#' cell_p_val_res@results
#'
#' # ANOVA raw p-value
#' natrual_units <- score_aov_pval |> dont_log_pvalues()
#' cell_pval_natrual_res <-
#'   natrual_units |>
#'   fit(class ~ ., data = cell_data)
#' cell_pval_natrual_res@results
#'
#' # ANOVA t/F-statistic
#' cell_t_stat_res <-
#'   score_aov_fstat |>
#'   fit(class ~ ., data = cell_data)
#' cell_t_stat_res@results
#'
#' # ---------------------------------------------------------------------------
#' library(dplyr)
#'
#' # Analysis of variance where `chem_fp_*` are the class predictors and
#' # `permeability` is the numeric outcome/response
#'
#' permeability <-
#'   modeldata::permeability_qsar |>
#'   # Make the problem a little smaller for time; use 50 predictors
#'   select(1:51) |>
#'   # Make the binary predictor columns into factors
#'   mutate(across(starts_with("chem_fp"), as.factor))
#'
#' perm_p_val_res <-
#'   score_aov_pval |>
#'   fit(permeability ~ ., data = permeability)
#' perm_p_val_res@results
#'
#' # Note that some `lm()` calls failed and are given NA score values. For
#' # example:
#' table(permeability$chem_fp_0007)
#'
#' perm_t_stat_res <-
#'   score_aov_fstat |>
#'   fit(permeability ~ ., data = permeability)
#' perm_t_stat_res@results
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
    transform_fn = function(x) x,
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
    transform_fn = function(x) x,
    direction = "maximize",
    deterministic = TRUE,
    tuning = FALSE,
    label = "ANOVA F-statistics"
  )

# ------------------------------------------------------------------------------

#' @export
S7::method(fit, class_score_aov) <- function(
  object,
  formula,
  data,
  case_weights = NULL,
  ...
) {
  analysis_data <- process_all_data(formula, data = data)
  predictors <- names(analysis_data)[-1]
  outcome <- names(analysis_data)[1]
  case_weights <- convert_weights(case_weights, nrow(analysis_data))

  complete_obs <- !is.na(analysis_data[outcome])
  if (!is.null(case_weights)) {
    complete_obs <- complete_obs & !is.na(case_weights)
  }
  analysis_data <- analysis_data[complete_obs, ]

  use_pval <- object@score_type == "aov_pval"
  score <- purrr::map_dbl(
    purrr::set_names(predictors),
    \(x) {
      map_score_aov(
        analysis_data,
        predictor = x,
        outcome = outcome,
        pval = use_pval,
        weights = case_weights
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
      object@direction <- "minimize"
    }
  }

  object@results <- res
  object
}

map_score_aov <- function(data, predictor, outcome, pval, weights) {
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

  complete_obs <- !is.na(predictor_col)
  outcome_col <- outcome_col[complete_obs]
  predictor_col <- predictor_col[complete_obs]

  if (!is.null(weights)) {
    weights <- weights[complete_obs]
  } else {
    weights <- rep(1.0, length(outcome_col))
  }

  res <- get_single_aov(predictor_col, outcome_col, pval, weights)
  res
}

get_single_aov <- function(predictor, outcome, pval = TRUE, weights = NULL) {
  flipped <- flip_if_needed_aov(predictor, outcome)
  outcome <- flipped$outcome
  predictor <- flipped$predictor
  df <- tibble::tibble(outcome = outcome, predictor = predictor)

  fit <- try(
    stats::lm(
      outcome ~ predictor,
      data = df,
      weights = weights,
      na.action = "na.omit"
    ),
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
