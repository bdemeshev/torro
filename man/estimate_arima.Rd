% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/estimate.R
\name{estimate_arima}
\alias{estimate_arima}
\title{Estimate auto arima model on multivariate time series}
\usage{
estimate_arima(y, ...)
}
\arguments{
\item{y}{multivariate time series}

\item{...}{further arguments passed to \code{auto.arima} function}
}
\value{
list of estimated arima models
}
\description{
\code{estimate_arima} returns a list of estimated arima model
}
\details{
Just a wrapper for \code{auto.arima} function from \code{forecast} package.
}
\examples{
data(rus_macro)
y_small <- rus_macro[, c("cpi", "employment", "m2")]
arima_model <- estimate_arima(y_small)
}
