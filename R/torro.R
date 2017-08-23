#' torro: project-package to forecast russian macro indicators with BVARs and alternative models
#'
#' Trying to test project-package idea.
#'
#' Trying to test project-package idea...
#'
#' @name torro
#' @docType package
NULL

#' Russian macro indicators
#'
#' Multivariate time-series with 23 variables. Monthly, deseasonalised. From 1995 to 2015.
#'
#' \itemize{
#' \item construction 
#' \item cpi
#' \item employment
#' \item export
#' \item gas_price
#' \item gov_balance
#' \item ib_rate
#' \item import
#' \item ind_prod
#' \item labor_request
#' \item lend_rate
#' \item m2
#' \item ner
#' \item nfa_cb
#' \item oil_price
#' \item ppi
#' \item real_income
#' \item real_investment
#' \item reer
#' \item retail
#' \item unemp_rate
#' \item wage
#' \item agriculture
#' }
#'
#' @docType data
#' @keywords datasets
#' @name rus_macro
#' @usage data(rus_macro)
#' @format Multivariate time-series, monthly, from 1995 to 2015, 23 variables
NULL

#' Variable sets 
#'
#' Tibble with the following columns
#'
#' \itemize{
#' \item var_set the name of variable set
#' \item variable names of variables in the variable set
#' \item pre_transform transformation before estimation (TODO)
#' \item post_transform transformation of forecasts (TODO)
#' }
#' See also toy version, \code{var_sets_toy}.
#'
#' @docType data
#' @keywords datasets
#' @name var_sets
#' @usage data(var_sets)
#' @format tibble
NULL

#' Variable sets toy version 
#'
#' Tibble with the following columns
#'
#' \itemize{
#' \item var_set the name of variable set
#' \item variable names of variables in the variable set
#' \item pre_transform transformation before estimation (TODO)
#' \item post_transform transformation of forecasts (TODO)
#' }
#' See also full version, \code{var_sets}.
#'
#' @docType data
#' @keywords datasets
#' @name var_sets_toy
#' @usage data(var_sets_toy)
#' @format tibble
NULL


#' Horizons toy version 
#'
#' Tibble with the following column
#'
#' \itemize{
#' \item h forecasting horizon
#' }
#' See also full version, \code{horizons}.
#'
#' @docType data
#' @keywords datasets
#' @name horizons_toy
#' @usage data(horizons_toy)
#' @format tibble
NULL

#' Horizons 
#'
#' Tibble with the following column
#'
#' \itemize{
#' \item h forecasting horizon
#' }
#' See also toy version, \code{horizons_toy}. 
#'
#' @docType data
#' @keywords datasets
#' @name horizons
#' @usage data(horizons)
#' @format tibble
NULL


#' Shifts 
#'
#' Tibble with the following columns
#'
#' \itemize{
#' \item shift_name name of the shift
#' \item shift_T_start starting time of the shift
#' \item win_expanding logical, TRUE for expanding/growing window, FALSE for moving
#' \item win_start_lenght length of the first window
#' \item n_shifts number of window shifts
#' }
#' See also toy version, \code{shifts_toy}. 
#'
#' @docType data
#' @keywords datasets
#' @name shifts
#' @usage data(shifts)
#' @format tibble
NULL

#' Shifts toy version
#'
#' Tibble with the following columns
#'
#' \itemize{
#' \item shift_name name of the shift
#' \item shift_T_start starting time of the shift
#' \item win_expanding logical, TRUE for expanding/growing window, FALSE for moving
#' \item win_start_lenght length of the first window
#' \item n_shifts number of window shifts
#' }
#' See also full version, \code{shifts}. 
#'
#' @docType data
#' @keywords datasets
#' @name shifts_toy
#' @usage data(shifts_toy)
#' @format tibble
NULL



