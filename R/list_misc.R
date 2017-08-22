#' Coerce gran_1 and gran_2 scalars in list into vector gran 
#' 
#' \code{granulatiry_to_vector} coerces gran_1 and gran_2 scalars in list into vector gran in the same list.
#' 
#' Coerces gran_1 and gran_2 scalars in list into vector gran in the same list.
#'  
#' @param df list with gran_1 and gran_2 scalar numbers
#' @return list with gran vector 
#' @export
#' @examples 
#' a <- list(gran_1 = 25, gran_2 = 10)
#' granularity_to_vector(a)
granularity_to_vector <- function(df) {
  df$gran <- c(df$gran_1, df$gran_2)
  df$gran_1 <- NULL
  df$gran_2 <- NULL
  return(df)
}




#' Set maximal horizon for h-agnostic models 
#' 
#' \code{set_agnostic_max_h} sets maximal horizon for h-agnostic models.
#' 
#' There are two types of models. Horizon agnostic that do not require horizon h for estimation (ARIMA, RW, ETS...). 
#' Horizon gnostic models that require h both for estimation and forecasting (cross validated VAR-lasso, ...). 
#' This function removes redundant estimation of h-agnostic models. If one requires estimation of h-agnostic
#' model for h = 1, h = 2, h = 3 this function will leave only estimation of h = 3 in the request.
#' List of models to estimate should contain at least
#' \itemize{
#' \item h forecasting horizon
#' \item h_required logical TRUE/FALSE, FALSE for h-agnostic models
#' }
#' All other variables except \code{ignored_vars} are used to identify whether models are different.
#'    
#' @param fits_long list of models to estimate
#' @param ignored_vars names of variables that are ignored 
#' when \code{set_agnostic_max_h} groups rows correponding to same model. 
#' @return shorter list of models to estimate 
#' @export
#' @examples 
#' fits_long <- tibble::tibble(h_required = c(TRUE, TRUE, FALSE, FALSE, FALSE),
#'    h = c(1, 2, 1, 2, 3), params = c(1, 1, 2, 2, 5))
#' set_agnostic_max_h(fits_long)
set_agnostic_max_h <- function(fits_long, ignored_vars = "model_filename") {
  h_required <- h <- NULL # black magic to remove NOTE in R CMD check
  
  fits_h_gnostic <- dplyr::filter(fits_long, h_required == TRUE)
  fits_h_agnostic <- dplyr::filter(fits_long, h_required == FALSE)
  
  group_by_vars <- setdiff(colnames(fits_long), c("h", ignored_vars))
  
  fits_h_agnostic <- dplyr::group_by_at(fits_h_agnostic, .vars = group_by_vars) 
  fits_h_agnostic <- dplyr::ungroup(dplyr::filter(fits_h_agnostic, h == max(h)))

  fits_long <- dplyr::bind_rows(fits_h_gnostic, fits_h_agnostic)
  return(fits_long)
}


#' Transform shifts data frame to samples data frame
#' 
#' \code{shifts_to_samples} transforms shifts data frame to samples data frame.
#' 
#' Transforms shifts data frame to samples data frame. Shifts data frame should contain
#' \itemize{
#' \item shift_name name of the shift
#' \item win_expanding logical, TRUE for growing window, FALSE for moving window
#' \item shift_T_start first T of the first window
#' \item win_start_length length of the first window
#' \item n_shifts number of window shifts
#' }
#' Samples data frame contains the same columns as shifts data frame plus three: T_start, T_end, sample_name. 
#'  
#' @param shifts shifts data frame
#' @return samples data frame. 
#' @export
#' @examples 
#' shifts <- tibble::tribble(~shift_name, ~shift_T_start, ~win_expanding, 
#'     ~win_start_length, ~n_shifts, "moving_120", 13, FALSE, 120, 2) 
#' shifts_to_samples(shifts)
shifts_to_samples <- function(shifts) {
  # black magic to remove NOTE in R CMD check:
  row_number <- shift_name <- shift_T_start <- NULL 
  shift_no <- T_end <- win_expanding <- NULL 
  win_start_length <- NULL
  
  line_no <- rep(1:nrow(shifts), times = shifts$n_shifts)
  samples <- shifts[line_no, ]
  
  samples <-  dplyr::group_by(samples, shift_name) 
  samples <- dplyr::mutate(samples, shift_no = row_number())
  
  samples <- dplyr::mutate(dplyr::ungroup(samples), 
           T_end = shift_T_start + win_start_length + shift_no - 2,
           T_start = T_end - win_start_length - win_expanding * (shift_no - 1) + 1,
           sample_name = paste0(shift_name, "_", shift_no))
  return(samples)
}


#' Get parameter column names
#' 
#' \code{get_parameter_colnames} gets parameter column names from fits_long data frame.
#' 
#' Variable "h" is considered as parameter. Just takes column names of fits_long and
#' removes column names used to identify model, shift, sample etc. 
#' One row of fits_long is sufficient as function works only with column names.
#'  
#' @param fits_long data frame with models to estimate.
#' @param redundant vector of column names NOT corresponding to parameters. 
#' If NULL then built-in vector is used.
#' @return vector of column names corresponding to parameters of models. 
#' @export
#' @examples 
#' fits_long <- tibble::tibble(h_required = c(TRUE, TRUE, FALSE, FALSE, FALSE),
#'    h = c(1, 2, 1, 2, 3), params = c(1, 1, 2, 2, 5))
#' get_parameter_colnames(fits_long)
get_parameter_colnames <- function(fits_long, redundant = NULL) {
  if (is.null(redundant)) {
    redundant <- c("shift_name", "shift_T_start", "win_expanding", "win_start_length", 
    "n_shifts", "shift_no", "T_end", "T_start", "sample_name", "model_type", 
    "model_args", "h_required", "comment", "var_set", "pars_id", 
    "expand_window_by", "result", "model_filename", "T_start_lower")
  }
  parameter_colnames <- setdiff(colnames(fits_long), redundant)
  return(parameter_colnames)
}


#' Try to estimate and forecast particular request
#' 
#' \code{forecast_one_fit} takes one row of fits_long and tries to forecast with corresponding model.
#' To protect against possible errors with complex models \code{try} is used.
#' 
#' Does not use parallel computations. 
#'  
#' @param fit_row one row from data frame with models to estimate.
#' @param var_sets data frame with variable names. Should contain at least two columns: 
#' \code{var_set} name of a set, 
#' \code{variable} names of the variables in the set.
#' @param ts_data multivariate time series
#' @param redundant vector of column names NOT corresponding to parameters. 
#' If NULL then built-in vector is used. See \code{get_parameter_colnames}.
#' @return forecasts from estimated model or \code{try-error} class 
#' @export
#' @examples 
#' # TODO
forecast_one_fit <- function(fit_row, var_sets,
                             ts_data = torro::rus_macro, redundant = NULL) {
  var_set <- NULL # black magic to remove NOTE from R CMD check
  
  parameter_colnames <- get_parameter_colnames(fit_row, redundant = redundant)
  parameters <- fit_row[, parameter_colnames]
  
  # remove NA parameters that are not required for actual fit:
  parameters <- as.list(parameters[, as.vector(!is.na(parameters[1, ]))])
  # concatenate two granularity parameters in one vector:
  if (fit_row$model_type == "var_lasso")  {
    parameters <- granularity_to_vector(parameters)
    parameters$type <- ifelse(fit_row$h_required, "honest", "fast")
  }
  if (!fit_row$h_required) {
    parameters$h <- NULL
  }
  
  forecast_fun_name <- paste0("forecast", "_", fit_row$model_type)
  forecast_fun <- eval(parse(text = forecast_fun_name))
  
  vars <- dplyr::filter(var_sets, var_set == fit_row$var_set)$variable
  y <- ts_data[(fit_row$T_start_lower):fit_row$T_end, vars]
  parameters$y <- y
  
  attempt <- try(do.call(forecast_fun, parameters))
  
  return(attempt)
}


#' Extract message from forecasting attempt
#' 
#' \code{attempt_to_status} extracts error message from estimation attempt.
#' 
#' Gives "OK" if attempt was successful. 
#'  
#' @param attempt estimation attempt.
#' @return character message
#' @export
#' @examples 
#' # TODO
attempt_to_status <- function(attempt) {
  if ("try-error" %in% class(attempt)) {
    status <- as.character(attempt) 
  } else {
    status <- "OK"
  }
  return(status)
}

