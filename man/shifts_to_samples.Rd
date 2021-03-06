% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/list_misc.R
\name{shifts_to_samples}
\alias{shifts_to_samples}
\title{Transform shifts data frame to samples data frame}
\usage{
shifts_to_samples(shifts)
}
\arguments{
\item{shifts}{shifts data frame}
}
\value{
samples data frame.
}
\description{
\code{shifts_to_samples} transforms shifts data frame to samples data frame.
}
\details{
Transforms shifts data frame to samples data frame. Shifts data frame should contain
\itemize{
\item shift_name name of the shift
\item win_expanding logical, TRUE for growing window, FALSE for moving window
\item shift_T_start first T of the first window
\item win_start_length length of the first window
\item n_shifts number of window shifts
}
Samples data frame contains the same columns as shifts data frame plus three: T_start, T_end, sample_name.
}
\examples{
shifts <- tibble::tribble(~shift_name, ~shift_T_start, ~win_expanding,
    ~win_start_length, ~n_shifts, "moving_120", 13, FALSE, 120, 2)
shifts_to_samples(shifts)
}
