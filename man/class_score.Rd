% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/class_score.R, R/score-aov.R, R/score-cor.R,
%   R/score-cross_tab.R, R/score-forest_imp.R, R/score-info_gain.R,
%   R/score-roc_auc.R
\name{class_score}
\alias{class_score}
\alias{class_score_aov}
\alias{class_score_cor}
\alias{class_score_xtab}
\alias{class_score_imp_rf}
\alias{class_score_info_gain}
\alias{class_score_roc_auc}
\title{General S7 classes for scoring objects}
\usage{
class_score(
  outcome_type = c("numeric", "factor"),
  predictor_type = c("numeric", "factor"),
  case_weights = logical(0),
  range = integer(0),
  inclusive = logical(0),
  fallback_value = integer(0),
  score_type = character(0),
  sorts = function() NULL,
  direction = character(0),
  deterministic = logical(0),
  tuning = logical(0),
  calculating_fn = function() NULL,
  label = character(0),
  packages = character(0),
  results = data.frame()
)

class_score_aov(
  outcome_type = c("numeric", "factor"),
  predictor_type = c("numeric", "factor"),
  case_weights = logical(0),
  range = integer(0),
  inclusive = logical(0),
  fallback_value = integer(0),
  score_type = character(0),
  sorts = function() NULL,
  direction = character(0),
  deterministic = logical(0),
  tuning = logical(0),
  calculating_fn = function() NULL,
  label = character(0),
  packages = character(0),
  results = data.frame(),
  neg_log10 = TRUE
)

class_score_cor(
  outcome_type = c("numeric", "factor"),
  predictor_type = c("numeric", "factor"),
  case_weights = logical(0),
  range = integer(0),
  inclusive = logical(0),
  fallback_value = integer(0),
  score_type = character(0),
  sorts = function() NULL,
  direction = character(0),
  deterministic = logical(0),
  tuning = logical(0),
  calculating_fn = function() NULL,
  label = character(0),
  packages = character(0),
  results = data.frame()
)

class_score_xtab(
  outcome_type = c("numeric", "factor"),
  predictor_type = c("numeric", "factor"),
  case_weights = logical(0),
  range = integer(0),
  inclusive = logical(0),
  fallback_value = integer(0),
  score_type = character(0),
  sorts = function() NULL,
  direction = character(0),
  deterministic = logical(0),
  tuning = logical(0),
  calculating_fn = function() NULL,
  label = character(0),
  packages = character(0),
  results = data.frame(),
  neg_log10 = TRUE,
  fdr = FALSE
)

class_score_imp_rf(
  outcome_type = c("numeric", "factor"),
  predictor_type = c("numeric", "factor"),
  case_weights = logical(0),
  range = integer(0),
  inclusive = logical(0),
  fallback_value = integer(0),
  score_type = character(0),
  sorts = function() NULL,
  direction = character(0),
  deterministic = logical(0),
  tuning = logical(0),
  calculating_fn = function() NULL,
  label = character(0),
  packages = character(0),
  results = data.frame(),
  engine = "ranger",
  trees = 100,
  mtry = 2,
  min_n = 1,
  mode = "classification",
  seed = 42
)

class_score_info_gain(
  outcome_type = c("numeric", "factor"),
  predictor_type = c("numeric", "factor"),
  case_weights = logical(0),
  range = integer(0),
  inclusive = logical(0),
  fallback_value = integer(0),
  score_type = character(0),
  sorts = function() NULL,
  direction = character(0),
  deterministic = logical(0),
  tuning = logical(0),
  calculating_fn = function() NULL,
  label = character(0),
  packages = character(0),
  results = data.frame(),
  mode = "classification"
)

class_score_roc_auc(
  outcome_type = c("numeric", "factor"),
  predictor_type = c("numeric", "factor"),
  case_weights = logical(0),
  range = integer(0),
  inclusive = logical(0),
  fallback_value = integer(0),
  score_type = character(0),
  sorts = function() NULL,
  direction = character(0),
  deterministic = logical(0),
  tuning = logical(0),
  calculating_fn = function() NULL,
  label = character(0),
  packages = character(0),
  results = data.frame()
)
}
\description{
\code{class_score} is an S7 object that contains slots for the characteristics of
predictor importance scores. More specific classes for individual methods are
based on this object (shown below).
}
\keyword{internal}
