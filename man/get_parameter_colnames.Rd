% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/list_misc.R
\name{get_parameter_colnames}
\alias{get_parameter_colnames}
\title{Get parameter column names}
\usage{
get_parameter_colnames(fits_long, redundant = NULL)
}
\arguments{
\item{fits_long}{data frame with models to estimate.}

\item{redundant}{vector of column names NOT corresponding to parameters.
If NULL then built-in vector is used.}
}
\value{
vector of column names corresponding to parameters of models.
}
\description{
\code{get_parameter_colnames} gets parameter column names from fits_long data frame.
}
\details{
Variable "h" is considered as parameter. Just takes column names of fits_long and
removes column names used to identify model, shift, sample etc.
One row of fits_long is sufficient as function works only with column names.
}
\examples{
fits_long <- tibble::tibble(h_required = c(TRUE, TRUE, FALSE, FALSE, FALSE),
   h = c(1, 2, 1, 2, 3), params = c(1, 1, 2, 2, 5))
get_parameter_colnames(fits_long)
}
