context("Forecasting models")

test_that("forecast_var_lasso test", {
  y_small <- rus_macro[, c("cpi", "employment")]
  var_lasso_forecast_x <- forecast_var_lasso(y_small, h = 1, scale = TRUE)
  f11 <- mforecast_to_matrix(var_lasso_forecast_x)[1, 1]
  res <- structure(4.97821746836293, .Names = "cpi")
  expect_equal(f11, res)
})