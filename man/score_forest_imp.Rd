% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/score-forest_imp.R
\name{score_forest_imp}
\alias{score_forest_imp}
\title{Create a score object for feature importance scores}
\usage{
score_forest_imp(
  range = c(0, Inf),
  fallback_value = Inf,
  score_type = "imp_rf",
  direction = "maximize",
  engine = "ranger",
  trees = 10,
  mtry = 2,
  min_n = 1,
  mode = "classification",
  seed = 42
)
}
\arguments{
\item{range}{A numeric vector of length two, specifying the minimum and maximum
possible values, respectively.}

\item{fallback_value}{A numeric scalar used as a fallback value. One of:
\itemize{
\item \code{Inf} (default)
}}

\item{score_type}{A character string indicating the type of scoring metric to compute.
One of:
\itemize{
\item \code{"imp_rf"} (default)
\item \code{"imp_rf_conditional"}
\item \code{"imp_rf_oblique"}
}}

\item{direction}{A character string indicating the optimization direction. One of:
\itemize{
\item \code{"maximize"} (default)
\item \code{"minimize"}
\item \code{"target"}
}}

\item{engine}{A character string specifying the random forest engine to use for fitting. One of:
\itemize{
\item \code{"ranger"} (default)
\item \code{"partykit"}
\item \code{"aorsf"}
}}

\item{trees}{An integer for the number of trees contained in the ensemble.}

\item{mtry}{An integer for the number of predictors that will
be randomly sampled at each split when creating the tree models.}

\item{min_n}{An integer for the minimum number of data points
in a node that are required for the node to be split further.}

\item{mode}{A character string indicating the task type. Relevant only for \code{ranger}. One of:
\itemize{
\item \code{"regression"}
\item \code{"classification"} (default)
}}

\item{seed}{An interger of random seed. Relevant only for \code{ranger}.}
}
\value{
A score object containing associated metadata such as \code{range}, \code{fallback_value},
\code{score_type} (\code{"imp_rf"}, \code{"imp_rf_conditional"}, \code{"imp_rf_oblique"}), \code{direction}, and other relevant attributes.
}
\description{
Construct a score object containing metadata for feature scoring using a
random forest, a conditional random forest or an oblique random forest
Output a score object containing associated metadata such as \code{range}, \code{fallback_value},
\code{score_type} (\code{"imp_rf"}, \code{"imp_rf_conditional"}, \code{"imp_rf_oblique"}), \code{direction}, and other relevant attributes.
}
\examples{
NULL
}
