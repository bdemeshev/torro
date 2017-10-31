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