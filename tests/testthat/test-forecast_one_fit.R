context("Forecast one fit tests...")

test_that("ARIMA", {
  fits_long <- create_fits_long(shifts, models, var_sets, horizons)
  fit_row <- fits_long[1, ]
  one_fit <- forecast_one_fit(fit_row, var_sets)
  forecast_matrix <- mforecast_to_matrix(one_fit)
  expect_equal(dim(forecast_matrix), c(12, 3))
})



test_that("LASSO", {
  fits_long <- create_fits_long(shifts, models, var_sets, horizons)
  fit_row <- fits_long[10, ]
  one_fit <- forecast_one_fit(fit_row, var_sets)
  forecast_matrix <- mforecast_to_matrix(one_fit)
  expect_equal(dim(forecast_matrix), c(12, 3))
})
