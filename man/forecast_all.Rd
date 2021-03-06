% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/list_misc.R
\name{forecast_all}
\alias{forecast_all}
\title{Forecast all requested models}
\usage{
forecast_all(fits_long, var_sets, n_cores = 1, basefolder,
  log_file = "estim_log.txt", ...)
}
\arguments{
\item{fits_long}{data frame with requested models}

\item{var_sets}{correspondance between variable sets and variable names}

\item{n_cores}{number of cores used. Just ignored.}

\item{basefolder}{path to folder where results are stored}

\item{log_file}{the name of log file}

\item{...}{further arguments passet to \code{forecast_one_fit}}
}
\value{
fits_long with estimation messages for each requested model.
}
\description{
\code{forecast_all} forecasts all requested models.
}
\details{
Just do it. This function may take a long time. If you stop it, you may
recover estimation status (column `result` in fits_long) from written fit files
using \code{get_status_from_fit_files()}.
It is better to clear basefolder before estimation process.
The function will just overwrite old `.Rds` files in case of name clash.
}
\examples{
fits_long_toy <- create_fits_long(shifts_toy, models_toy, var_sets_toy, horizons_toy)
fits_long_very_toy <- fits_long_toy[1:2, ]
res <- forecast_all(fits_long_very_toy, var_sets_toy, n_cores = 1,
    basefolder = tempdir())
}
