% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/score-aov.R
\name{score_aov}
\alias{score_aov}
\title{Create a score object for ANOVA F-test F-statistics and p-values}
\usage{
score_aov(
  range = c(0, Inf),
  fallback_value = Inf,
  score_type = "fstat",
  direction = "maximize",
  neg_log10 = TRUE
)
}
\arguments{
\item{range}{A numeric vector of length two, specifying the minimum and maximum
possible values, respectively.}

\item{fallback_value}{A numeric scalar used as a fallback value. One of:
\itemize{
\item \code{0}
\item \code{Inf} (default)
}

For F-statistics, the \code{fallback_value} is \code{"Inf"}. For p-values,
since the default applies a negative log10 transformation to p-values,
the \code{fallback_value} is \code{"Inf"}.}

\item{score_type}{A character string indicating the type of scoring metric to compute.
One of:
\itemize{
\item \code{"fstat"} (default)
\item \code{"pval"}
}}

\item{direction}{A character string indicating the optimization direction. One of:
\itemize{
\item \code{"maximize"} (default)
\item \code{"minimize"}
\item \code{"target"}
}

For F-statistics, the \code{direction} is \code{"maximize"}. For p-values,
since the default applies a negative log10 transformation to p-values,
the \code{direction} is \code{"maximize"}.}

\item{neg_log10}{A logical value indicating whether to apply a negative log10
transformation to p-values. One of:
\itemize{
\item \code{TRUE} (default)
\item \code{FALSE}
}

If \code{TRUE}, p-values are transformed as \code{-log10(pval)}. In this case:
\itemize{
\item The \code{fallback_value} is \code{Inf} (default)
\item The \code{direction} is \code{"maximize"} (default)
}

If \code{FALSE}, raw p-values are used. In this case:
\itemize{
\item The \code{fallback_value} needs to be set to \code{0}
\item The \code{direction} needs to be set to \code{"minimize"}
}}
}
\value{
A score object containing associated metadata such as \code{range}, \code{fallback_value},
\code{score_type} (\code{"fstat"} or \code{"pval"}), \code{direction}, and other relevant attributes.
}
\description{
Construct a score object containing metadata for univariate feature scoring using the
ANOVA F-test.
Output a score object containing associated metadata such as \code{range}, \code{fallback_value},
\code{score_type} (\code{"fstat"} or \code{"pval"}), \code{direction}, and other relevant attributes.
}
\examples{
# Create a score object
score_aov()
# Change score type to use -log10(p-values)
score_aov(score_type = "pval")
# Change score type to use raw p-values
score_aov(
  score_type = "pval",
  neg_log10 = FALSE,
  direction = "minimize",
  fallback_value = 0
)
}
