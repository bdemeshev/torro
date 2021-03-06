#' Read forecasts from fit files
#'
#' \code{get_forecasts_from_fit_files} reads forecasts from fit files.
#'
#' Read forecasts from fit files.
#'
#' @param basefolder folder with subfolder fits with fit files.
#' @param fit_file_names the names of fit files. If not specified all files are used.
#' @export
#' @return tibble with forecasts
get_forecasts_from_fit_files <- function(fit_file_names = NULL, basefolder) {
  if (is.null(fit_file_names)) {
    fit_file_names <- list.files(paste0(basefolder, "/fits/"), pattern = "\\.Rds$")
  }
  full_fit_file_path <- paste0(basefolder, "/fits/", fit_file_names)

  all_forecasts <- NULL
  for (file_name in full_fit_file_path) {
    model_id <- as.numeric(stringr::str_extract(basename(file_name), "[0-9]+"))
    one_fit <- readr::read_rds(file_name)
    if ("mforecast" %in% class(one_fit)) {
      forecast_matrix <- mforecast_to_matrix(one_fit)
      forecast_tibble <- tibble::as.tibble(forecast_matrix)
      forecast_tibble$model_id <- model_id
      forecast_tibble$row_number <- 1:nrow(forecast_tibble)
      forecast_melted <- reshape2::melt(forecast_tibble, id.vars = c("row_number", "model_id"))
      all_forecasts <- dplyr::bind_rows(all_forecasts, forecast_melted)
    }
  }
  return(all_forecasts)
}

#' Attach real y and model information to all_forecasts data set.
#'
#' Add new columns to all_forecasts with short model description and actual y.
#'
#' Original all_forecasts contains row_number, model_id, variable and value columns.
#' Using info from fits_long this function attach actual y.
#'
#' @param all_forecasts tibble that contains row_number, model_id, variable and value columns.
#' @param fits_long tibble that contains T_end, model_type and model_filename columns
#' @param y actual time series
#' @export
#' @return all_forecasts with new columns: y_actual, var_set, pars_id...
augment_forecasts <- function(all_forecasts, fits_long, y) {
  fits_long <- dplyr::mutate(fits_long,
                             model_id = as.numeric(stringr::str_extract(model_filename, "[0-9]+")))
  # dirty hack to remove notes in R CMD check
  model_filename <- NULL
  T_end <- NULL
  model_type <- NULL
  model_id <- NULL
  row_number <- NULL
  var_set <- NULL
  pars_id <- NULL

  fits_long <- dplyr::select(fits_long, T_end, model_type, model_id, var_set, pars_id)
  all_forecasts <- dplyr::left_join(all_forecasts, fits_long, by = "model_id")
  all_forecasts <- dplyr::mutate(all_forecasts, T_forecast = T_end + row_number)
  y <- dplyr::as_tibble(y)
  y <- dplyr::mutate(y, T_forecast = 1:nrow(y))

  y_melted <- reshape2::melt(y, id.vars = "T_forecast", value.name = "y_actual")
  y_melted$variable <- as.character(y_melted$variable)

  all_forecasts <- dplyr::left_join(all_forecasts, y_melted, by = c("T_forecast", "variable"))
  return(all_forecasts)
}


#' Calculate rmse and mae.
#'
#' Create summarising table with rmse and mae for each model.
#'
#' Further checks are needed!
#'
#' @param all_forecasts tibble that contains row_number, model_id, variable, value and y_actual
#' @param fits_long tibble that contains pars_id, var_set and model_filename columns
#' @param T_rmse_min first T to calculate rmse
#' @param T_rmse_max last T to calculate rmse
#' @export
#' @return summary with rmse and mae for each model
calculate_rmse <- function(all_forecasts, fits_long,
                                  T_rmse_min = 133,
                                  T_rmse_max = 243) {
  # dirty hack to remove notes in R CMD check
  model_filename <- NULL
  model_id <- NULL
  row_number <- NULL
  var_set <- NULL
  pars_id <- NULL
  T_forecast <- NULL
  y_actual <- NULL
  value <- NULL
  variable <- NULL
  error_2 <- NULL
  error_abs <- NULL
  model_type <- NULL
  mse <- NULL


  all_f3 <- dplyr::filter(all_forecasts, T_forecast >= T_rmse_min, T_forecast <= T_rmse_max)

  all_f3 <- dplyr::mutate(all_f3,
                   error_2 = (value - y_actual)^2,
                   error_abs = abs(value - y_actual))

  rmse_table <- dplyr::summarise(dplyr::group_by(all_f3,
          model_type, pars_id, row_number, variable, var_set),
                     mse = mean(error_2),
                     rmse = sqrt(mse),
                     mae = mean(error_abs))


  return(rmse_table)
}


#' Transform absolute error measures to relative.
#'
#' Transform absolute error measures to relative.
#'
#' Transform absolute error measures to relative.
#'
#' @param rmse_table output of `calculate_rmse` function.
#' Should contain columns: row_number /horizon/, variable, var_set, model_type, pars_id
#' and some measures like mse, rmse etc
#' @param the_model_type base model type
#' @param the_pars_id base parameter set
#' @export
#' @return measure table with relative measures
transform_to_relative_measure <- function(rmse_table,
                                          the_model_type = "rw",
                                          the_pars_id = "automatic") {
  # dirty hack to remove notes in R CMD check
  model_type <- NULL
  pars_id <- NULL
  value <- NULL
  base_value <- NULL

  rmse_table <- dplyr::ungroup(rmse_table)
  rmse_table_long <- reshape2::melt(rmse_table,
                                    id.vars = c("row_number", "variable", "var_set", "model_type", "pars_id"),
                                    variable.name = "measure")
  rmse_table_long_base <- dplyr::filter(rmse_table_long, model_type == the_model_type, the_pars_id == pars_id)
  rmse_table_long_base <- dplyr::select(rmse_table_long_base, -model_type, -pars_id)
  rmse_table_long_base <- dplyr::rename(rmse_table_long_base, base_value = value)


  rmse_table_long_joined <- dplyr::left_join(rmse_table_long, rmse_table_long_base,
                                             by = c("variable", "var_set", "row_number", "measure"))
  rmse_table_long_joined <- dplyr::mutate(rmse_table_long_joined,
                                          rel_value = value / base_value)
  new_rmse_table <- reshape2::dcast(rmse_table_long_joined,
                                    row_number + variable + var_set + model_type + pars_id ~ measure,
                                    value.var = "rel_value")

  return(new_rmse_table)
}


