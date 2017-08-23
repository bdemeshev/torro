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


#' Arguments for random walk models
#'
#' Tibble with the following columns
#'
#' \itemize{
#' \item pars_id id of parameter combination
#' \item expand_window_by number of observations added to basic window length.
#' Usually this number is equal to the number of parameters in the model. One for RW model with drift.
#' }
#'
#' @docType data
#' @keywords datasets
#' @name arguments_rw
#' @usage data(arguments_rw)
#' @format tibble
NULL

#' Arguments for auto arima models
#'
#' Tibble with the following columns
#'
#' \itemize{
#' \item pars_id id of parameter combination
#' \item expand_window_by number of observations added to basic window length.
#' Usually this number is equal to the number of parameters in the model. Three for an average ARIMA model.
#' Can not be computed beforehand for auto.arima because the number of parameters is not known a-priori.
#' }
#'
#' @docType data
#' @keywords datasets
#' @name arguments_arima
#' @usage data(arguments_arima)
#' @format tibble
NULL


#' Arguments for ETS models
#'
#' Tibble with the following columns
#'
#' \itemize{
#' \item pars_id id of parameter combination
#' \item expand_window_by number of observations added to basic window length.
#' Usually this number is equal to the number of parameters in the model. Three for an average ETS model.
#' Can not be computed beforehand for \code{ets} because the number of parameters is not known a-priori.
#' }
#'
#' @docType data
#' @keywords datasets
#' @name arguments_ets
#' @usage data(arguments_ets)
#' @format tibble
NULL


#' Arguments for VAR-lasso models
#'
#' Tibble with the following columns
#'
#' \itemize{
#' \item p number of lags
#' \item struct LASSO lag structure
#' Three top players according to \url{https://arxiv.org/pdf/1508.07497.pdf} page 23 are
#' SparseLag (Lag Sparse Group VARX-L), OwnOther (Own/Other Group VARX-L), SparseOO (Own/Other Sparse Group VARX-L).
#' but SparseLag and SparseOO hangs the computer.
#' \item gran_1 first granularity parameter: 
#' left grid search border is equal to maximal lambda / gran_1. 
#' Maximal labmda sets all coefficient estimates to zero.
#' \item gran_2 second granularity parameter: number of grid points for cross-validation.
#' Values gran_1 = 25 and grand_2 = 10 are suggested by BigVAR user's guide, 
#' \url{http://www.wbnicholson.com/BigVAR.pdf}, page 9.
#' \item pars_id id of parameter combination
#' \item expand_window_by number of observations added to basic window length.
#' Usually this number is equal to the number of parameters in the model. Three for an average ARIMA model.
#' Can not be computed beforehand for auto.arima because the number of parameters is not known a-priori.
#' }
#' See also toy version, \code{arguments_var_lasso_toy}.
#'
#' @docType data
#' @keywords datasets
#' @name arguments_var_lasso
#' @usage data(arguments_var_lasso)
#' @format tibble
NULL


#' Arguments for VAR-lasso models toy version
#'
#' Tibble with the following columns
#'
#' \itemize{
#' \item p number of lags
#' \item struct LASSO lag structure
#' Three top players according to \url{https://arxiv.org/pdf/1508.07497.pdf} page 23 are
#' SparseLag (Lag Sparse Group VARX-L), OwnOther (Own/Other Group VARX-L), SparseOO (Own/Other Sparse Group VARX-L).
#' but SparseLag and SparseOO hangs the computer.
#' \item gran_1 first granularity parameter: 
#' left grid search border is equal to maximal lambda / gran_1. 
#' Maximal labmda sets all coefficient estimates to zero.
#' \item gran_2 second granularity parameter: number of grid points for cross-validation.
#' Values gran_1 = 25 and grand_2 = 10 are suggested by BigVAR user's guide, 
#' \url{http://www.wbnicholson.com/BigVAR.pdf}, page 9.
#' \item pars_id id of parameter combination
#' \item expand_window_by number of observations added to basic window length.
#' Usually this number is equal to the number of parameters in the model. Three for an average ARIMA model.
#' Can not be computed beforehand for auto.arima because the number of parameters is not known a-priori.
#' }
#' See also full version, \code{arguments_var_lasso}.
#'
#' @docType data
#' @keywords datasets
#' @name arguments_var_lasso_toy
#' @usage data(arguments_var_lasso_toy)
#' @format tibble
NULL



#' All models with arguments in a nested data frame
#'
#' Tibble with the following columns
#'
#' \itemize{
#' \item model_type type of a model
#' \item comment hm, comment
#' \item h_required logical, TRUE if estimation of models depends on forecasting horizon h.
#' For example \code{auto.arima} estimation does not depend on h, 
#' but cross-validation of VAR lasso does depend on h.
#' \item model_args tibble with arguments for the model except h. 
#' For example, number of lags p for VAR lasso model.
#' }
#' See also toy version, \code{models_toy}.
#'
#' @docType data
#' @keywords datasets
#' @name models
#' @usage data(models)
#' @format tibble
NULL


#' All models with arguments in a nested data frame toy version
#'
#' Tibble with the following columns 
#'
#' \itemize{
#' \item model_type type of a model
#' \item comment hm, comment
#' \item h_required logical, TRUE if estimation of models depends on forecasting horizon h.
#' For example \code{auto.arima} estimation does not depend on h, 
#' but cross-validation of VAR lasso does depend on h.
#' \item model_args tibble with arguments for the model except h. 
#' For example, number of lags p for VAR lasso model.
#' }
#' See also full version, \code{models}.
#'
#' @docType data
#' @keywords datasets
#' @name models_toy
#' @usage data(models_toy)
#' @format tibble
NULL



