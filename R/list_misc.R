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
    "expand_window_by", "result", "model_filename", "T_start_lower", "border_optimum")
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
#' fits_long_toy <- create_fits_long(shifts_toy, models_toy, var_sets_toy, horizons_toy)
#' fit_row <- fits_long_toy[1, ]
#' attempt <- forecast_one_fit(fit_row, var_sets)
forecast_one_fit <- function(fit_row, var_sets,
                             ts_data = torro::rus_macro, redundant = NULL) {
  var_set <- NULL # black magic to remove NOTE from R CMD check


  # do not pass what is not a parameter
  parameter_colnames <- get_parameter_colnames(fit_row, redundant = redundant)
  parameters <- fit_row[, parameter_colnames]

  # remove NA parameters that are not required for actual fit:
  parameters <- as.list(parameters[, as.vector(!is.na(parameters[1, ]))])
  # concatenate two granularity parameters in one vector:
  if (fit_row$model_type == "var_lasso")  {
    parameters <- granularity_to_vector(parameters)
    parameters$type <- ifelse(fit_row$h_required, "honest", "fast")
  }

  # this is a bug!!! protection against passing h should be before the estimation
  # if (!fit_row$h_required) {
  #   parameters$h <- NULL
  # }

  forecast_fun_name <- paste0("forecast", "_", fit_row$model_type)
  forecast_fun <- eval(parse(text = forecast_fun_name))


  # do not pass what is not a parameter
  # bad protection! will remove what is passed in ...
  # possible bug!!!!!
  # forecast_fun_args <- names(formals(forecast_fun))
  # parameters <- parameters[base::intersect(names(parameters), forecast_fun_args)]

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
#' fits_long_toy <- create_fits_long(shifts_toy, models_toy, var_sets_toy, horizons_toy)
#' fit_row <- fits_long_toy[1, ]
#' attempt <- forecast_one_fit(fit_row, var_sets)
#' attempt_to_status(attempt)
attempt_to_status <- function(attempt) {
  if ("try-error" %in% class(attempt)) {
    status <- as.character(attempt)
  } else {
    status <- "OK"
  }
  return(status)
}

#' Log message for boring estimation
#'
#' Message is returned and (possibly) written to file.
#'
#' May include additional message.
#'
#' @param fit_row one row data.frame with at least model_type column
#' @param full_path_to_log_file maybe empty
#' @param additional_message additional message
#' @return message message
#' @export
#' @examples
#' fits_long_toy <- create_fits_long(shifts_toy, models_toy, var_sets_toy, horizons_toy)
#' log_message(fits_long_toy[1, ], tempfile(), "Hi, there!\n")
log_message <- function(fit_row, full_path_to_log_file, additional_message = "") {
  message_1 <- paste0(utils::timestamp(quiet = TRUE), "\n")
  message_2 <- additional_message
  message_3 <- paste0("Model type: ", fit_row$model_type, "\n")
  message <- paste0(message_1, message_2, message_3)
  cat(message, file = full_path_to_log_file, append = TRUE)
  return(message)
}


#' Forecast all requested models
#'
#' \code{forecast_all} forecasts all requested models.
#'
#' Just do it. This function may take a long time. If you stop it, you may
#' recover estimation status (column `result` in fits_long) from written fit files
#' using \code{get_status_from_fit_files()}.
#' It is better to clear basefolder before estimation process.
#' The function will just overwrite old `.Rds` files in case of name clash.
#'
#' @param fits_long data frame with requested models
#' @param var_sets correspondance between variable sets and variable names
#' @param n_cores number of cores used. Just ignored.
#' @param ... further arguments passet to \code{forecast_one_fit}
#' @param basefolder path to folder where results are stored
#' @param log_file the name of log file
#' @return fits_long with estimation messages for each requested model.
#' @export
#' @examples
#' fits_long_toy <- create_fits_long(shifts_toy, models_toy, var_sets_toy, horizons_toy)
#' fits_long_very_toy <- fits_long_toy[1:2, ]
#' res <- forecast_all(fits_long_very_toy, var_sets_toy, n_cores = 1,
#'     basefolder = tempdir())
forecast_all <- function(fits_long, var_sets, n_cores = 1,
                         basefolder, log_file = "estim_log.txt", ...) {

  # dirty hack to remove R CMD NOTE:
  fit_no <- NULL

  dir.create(paste0(basefolder, "/fits"), recursive = TRUE)

  full_path_to_log_file <- paste0(basefolder, "/", log_file)

  if (n_cores > 1) {
    cluster <- parallel::makeCluster(n_cores, outfile = full_path_to_log_file)
    doParallel::registerDoParallel(cluster)

    # additional functions/data to pass for each cluster:
    export_functions <- NULL # NULL for nothing
    export_packages <- c("readr", "forecast", "BigVAR", "dplyr", "torro")

    # strange dirty hack from
    # https://stackoverflow.com/questions/30216613
    `%go%` <- foreach::`%dopar%`

    estimation_result <- foreach::foreach(fit_no = 1:nrow(fits_long),
                   .export = export_functions,
                   .packages = export_packages,
                   .combine = "c") %go% {

      fit_row <- fits_long[fit_no, ]

      additional_message <- paste0("Processing fit no ", fit_no,
                                 " out of ", nrow(fits_long), "\n")
      message <- log_message(fit_row, full_path_to_log_file, additional_message)

      attempt <- forecast_one_fit(fit_row, var_sets, ...)

      status <- attempt_to_status(attempt)

      if (status == "OK") {
        readr::write_rds(attempt, path = paste0(basefolder, "/fits/", fit_row$model_filename))
      }

      invisible(status)
    }

    parallel::stopCluster(cluster)
  } else {
    # non-parallel version
    estimation_result <- rep("", nrow(fits_long))

    for (fit_no in 1:nrow(fits_long)) {
      fit_row <- fits_long[fit_no, ]

      additional_message <- paste0("Processing fit no ", fit_no,
                                   " out of ", nrow(fits_long), "\n")
      message <- log_message(fit_row, full_path_to_log_file, additional_message)

      attempt <- forecast_one_fit(fit_row, var_sets, ...)

      estimation_result[fit_no] <- attempt_to_status(attempt)

      if (estimation_result[fit_no] == "OK") {
        readr::write_rds(attempt, path = paste0(basefolder, "/fits/", fit_row$model_filename))
      }
    }
  }

  fits_long$result <- estimation_result

  readr::write_rds(fits_long, path = paste0(basefolder, "/fits_long.Rds"))
  return(fits_long)
}


#' Create fits_long data frame
#'
#' \code{create_fits_long} creates fits_long data frame.
#'
#' Basically it is external product of \code{samples}, \code{var_sets} and \code{horizons} data frames plus
#' creation of columns \code{result}, \code{model_filename}, \code{T_start_lower}.
#'
#' @param shifts shifts data frame
#' @param var_sets var_sets data frame
#' @param models models data frame
#' @param horizons horizons data frame
#' @return fits_long data frame
#' @export
#' @examples
#' fits_long_toy <- create_fits_long(shifts_toy, models_toy, var_sets_toy, horizons_toy)
create_fits_long <- function(shifts, models, var_sets, horizons) {
  # black magic to remove NOTE in R CMD check
  T_start <- expand_window_by <- T_start <- row_number <- NULL

  samples <- shifts_to_samples(shifts)

  fits <- tidyr::crossing(samples, models, var_set = var_sets$var_set, horizons)
  fits_long <- tidyr::unnest(fits)

  fits_long <- set_agnostic_max_h(fits_long)

  # add model_filename (used as id) just before cycle
  fits_long <-  dplyr::mutate(fits_long, result = "Non-estimated",
                        model_filename = paste0("fit_", row_number(), ".Rds"),
                        T_start_lower = T_start - expand_window_by)
  return(fits_long)
}



#' Recover estimation status from fit file
#'
#' \code{get_status_from_fit_file} recovers estimation status from fit file.
#'
#' Recovers estimation status from one fit file. Internal function.
#'
#' @param basefolder folder with subfolder fits with fit files.
#' @param fit_file_name the name of fit file.
#' @return status message.
get_status_from_fit_file <- function(fit_file_name, basefolder) {
  full_fit_file_path <- paste0(basefolder, "/fits/", fit_file_name)
  if (!file.exists(full_fit_file_path)) {
    status <- "Non-estimated"
  } else {
    attempt <- readr::read_rds(full_fit_file_path)
    status <- attempt_to_status(attempt)
  }
  return(status)
}

#' Recover estimation status from fit files
#'
#' \code{get_status_from_fit_files} recovers estimation status from fit file.
#'
#' Recovers estimation status from fit files.
#' Vectorised version of \code{get_status_from_fit_file()}.
#'
#' @param basefolder folder with subfolder fits with fit files.
#' @param fit_file_name fit file names.
#' @return status messages.
#' @export
#' @examples
#' get_status_from_fit_files(c("fit_42.Rds", "fit_14.Rds"), tempdir())
get_status_from_fit_files <- function(fit_file_name, basefolder) {
  status <- purrr::map_chr(fit_file_name,
                          ~get_status_from_fit_file(., basefolder))
  return(status)
}



#' Check whether BigVAR LASSO optimum landed at border
#'
#' \code{is_optimum_at_border} checks whether BigVAR LASSO optimum landed at border.
#'
#' Takes mforecast objects, extracts model.
#' Than checks whether optimum is equal to the minimal lambda considered.
#' Returns FALSE for non BigVAR LASSO models.
#'
#' @param fit object of type mforecast.
#' @return TRUE/FALSE.
#' @export
#' @examples
#' fits_long_toy <- create_fits_long(shifts_toy, models_toy, var_sets_toy, horizons_toy)
#' fit_row <- fits_long_toy[1, ]
#' attempt <- forecast_one_fit(fit_row, var_sets)
#' is_optimum_at_border(attempt)
is_optimum_at_border <- function(fit) {
  optimum_at_border <- FALSE
  model <- fit$model
  if ("BigVAR.results" %in% class(model)) {
    grid_length <- length(model@LambdaGrid)
    optimal_lambda_index <- model@index
    if (grid_length == optimal_lambda_index) {
      optimum_at_border <- TRUE
    }
  } else {
    message("Class of the (...)$model does not contain 'BigVAR.results'")
  }
  return(optimum_at_border)
}


#' Get optimum at border flag from fit file
#'
#' \code{get_optimum_at_border_flag} gets optimum at border flag.
#'
#' Recovers optimum at border flag from one fit file. Internal function.
#'
#' @param basefolder folder with subfolder fits with fit files.
#' @param fit_file_name the name of fit file.
#' @return optimum at border flag, TRUE/FALSE.
get_optimum_at_border_flag <- function(fit_file_name, basefolder) {
  full_fit_file_path <- paste0(basefolder, "/fits/", fit_file_name)
  if (!file.exists(full_fit_file_path)) {
    optimum_at_border_flag <- FALSE
  } else {
    attempt <- readr::read_rds(full_fit_file_path)
    optimum_at_border_flag <- is_optimum_at_border(attempt)
  }
  return(optimum_at_border_flag)
}

#' Get optimum at border flags
#'
#' \code{get_optimum_at_border_flags} gets optimum at border flags.
#'
#' Recovers optimum at border flags from fit files.
#'
#' @param basefolder folder with subfolder fits with fit files.
#' @param fit_file_name fit file names.
#' @return vector of TRUE/FALSE.
#' @export
#' @examples
#' get_optimum_at_border_flags(c("fit_42.Rds", "fit_14.Rds"), tempdir())
get_optimum_at_border_flags <- function(fit_file_name, basefolder) {
  optimum_at_border_flags <- purrr::map_chr(fit_file_name,
                           ~get_optimum_at_border_flag(., basefolder))
  return(optimum_at_border_flags)
}
