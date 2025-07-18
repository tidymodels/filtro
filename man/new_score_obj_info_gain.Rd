% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/score-info_gain.R
\name{new_score_obj_info_gain}
\alias{new_score_obj_info_gain}
\title{Construct a subclassed score object for information gain with additional metadata}
\usage{
new_score_obj_info_gain(
  outcome_type = c("numeric", "factor"),
  predictor_type = c("numeric", "factor"),
  case_weights = logical(0),
  range = integer(0),
  inclusive = logical(0),
  fallback_value = integer(0),
  score_type = character(0),
  trans = function() NULL,
  sorts = function() NULL,
  direction = character(0),
  deterministic = logical(0),
  tuning = logical(0),
  ties = logical(0),
  calculating_fn = function() NULL,
  label = character(0),
  results = data.frame(),
  mode = "classification"
)
}
\arguments{
\item{outcome_type}{A character string indicating the outcome type. One of:
\itemize{
\item \code{"numeric"}
\item \code{"factor"}
}}

\item{predictor_type}{A character string indicating the predictor type. One of:
\itemize{
\item \code{"numeric"}
\item \code{"factor"}
}}

\item{case_weights}{A logical value, indicating whether the model accepts
case weights (\code{TRUE}) or not (\code{FALSE}).}

\item{range}{A numeric vector of length two, specifying the minimum and maximum
possible values, respectively.}

\item{inclusive}{A logical vector of length two, indicating whether the lower and
upper bounds of the range are inclusive (\code{TRUE}) or exclusive (\code{FALSE}).}

\item{fallback_value}{A numeric scalar used as a fallback value. One of:
\itemize{
\item \code{1}
\item \code{Inf} (default)
}}

\item{score_type}{A character string indicating the type of scoring metric to compute.
One of:
\itemize{
\item \code{"infogain"} (default)
\item \code{"gainratio"}
\item \code{"symuncert"}
}}

\item{trans}{NULL}

\item{sorts}{An optional function used to sort the scores. Common options include:
\itemize{
\item \code{identity}
\item \code{abs}
\item \code{function(score) max(score, 1 - score)}
}}

\item{direction}{A character string indicating the optimization direction. One of:
\itemize{
\item \code{"maximize"} (default)
\item \code{"minimize"}
\item \code{"target"}
}}

\item{deterministic}{A logical value, indicating whether the score is
deterministic (\code{TRUE}) or not (\code{FALSE}).}

\item{tuning}{A logical value, indicating whether the model should be tuned
(\code{TRUE}) or not (\code{FALSE}).}

\item{ties}{An optional logical value indicating whether ties in score can occur (\code{TRUE})
or not (\code{FALSE}).}

\item{calculating_fn}{An optional function used to compute the score. A default function
is selected based on the \code{score_type}.}

\item{label}{A named character string that can be used for printing and plotting.}

\item{results}{A tibble of result with one row per predictor, and four columns:
\itemize{
\item \code{name}: the name of scoring metric.
\item \code{score}: the score for the predictor-outcome pair.
\item \code{predictor}: the name of the predictor.
\item \code{outcome}: the name of the outcome.
}}

\item{mode}{A character string indicating the task type. One of:
\itemize{
\item \code{"regression"}
\item \code{"classification"} (default)
}}
}
\value{
A score object containing associated metadata such as \code{range}, \code{fallback_value},
\code{score_type}, \code{direction}, and other relevant attributes.
}
\description{
Introduce a new properties \code{is_reg}.
Output a new score object that contains associated metadata, such as \code{range},
\code{fallback_value}, \code{score_type}, \code{direction}, and other relevant attributes.
}
\examples{
# Create a score object
new_score_obj_info_gain()
}
