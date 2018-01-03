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
    model_id <- as.numeric(stringr::str_extract(file_name, "[0-9]+"))
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

#' Attach real y to all_forecasts data set.
#'
#' Add new column to all_forecasts with short model description and actual y.
#'
#' Original all_forecasts contains row_number, model_id, variable and value columns.
#' Using info from fits_long this function attach actual y.
#'
#' @param all_forecasts tibble that contains row_number, model_id, variable and value columns.
#' @param fits_long tibble that contains T_end, model_type and model_filename columns
#' @param y actual time series
#' @export
#' @return all_forecasts with new column
attach_y_to_forecasts <- function(all_forecasts, fits_long, y) {
  fits_long <- dplyr::mutate(fits_long,
                             model_id = as.numeric(stringr::str_extract(model_filename, "[0-9]+")))
  # dirty hack to remove notes in R CMD check
  model_filename <- NULL
  T_end <- NULL
  model_type <- NULL
  model_id <- NULL
  row_number <- NULL

  fits_long <- dplyr::select(fits_long, T_end, model_type, model_id)
  all_forecasts <- dplyr::left_join(all_forecasts, fits_long, by = "model_id")
  all_forecasts <- dplyr::mutate(all_forecasts, T_forecast = T_end + row_number)
  y <- dplyr::as_tibble(y)
  y <- dplyr::mutate(y, T_forecast = 1:nrow(y))

  y_melted <- reshape2::melt(y, id.vars = "T_forecast", value.name = "y_actual")
  y_melted$variable <- as.character(y_melted$variable)

  all_forecasts <- dplyr::left_join(all_forecasts, y_melted, by = c("T_forecast", "variable"))
  return(all_forecasts)
}




