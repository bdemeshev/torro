% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/torro.R
\docType{data}
\name{models}
\alias{models}
\title{All models with arguments in a nested data frame}
\format{tibble}
\usage{
data(models)
}
\description{
Tibble with the following columns
}
\details{
\itemize{
\item model_type type of a model
\item comment hm, comment
\item h_required logical, TRUE if estimation of models depends on forecasting horizon h.
For example \code{auto.arima} estimation does not depend on h, 
but cross-validation of VAR lasso does depend on h.
\item model_args tibble with arguments for the model except h. 
For example, number of lags p for VAR lasso model.
}
See also toy version, \code{models_toy}.
}
\keyword{datasets}
