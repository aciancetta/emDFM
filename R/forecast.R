

#' Get forecasts form the estimated DFM
#'
#' @param obj_fit A list of estimated parameters of the Dynamic Factor Model, as
#' obtained from `pca_estimator()`, `kalman_filter()`, `kalman_smoother()`, `em_algorithm()`
#' @param horizon An integer. Number of steps-ahead of the forecast. Default is 1.
#'
#' @return A vector of `horizon`-steps-ahead forecasts for all the variables in the original dataset
#' @export
#'
#' @examples
#' # d <- download_clean_data("IT")
#' # em_fit <- em_algorithm(d)
#' # forecast(em_fit, horizon = 1)
#'
forecast <- function(obj_fit, horizon = 1){

  `%^%` <-  expm::`%^%`

  Ftt <- obj_fit$factors
  Lambda <- obj_fit$parameters$Lambda
  p <- obj_fit$parameters$p
  r <- ncol(Ftt)
  A_list <- obj_fit$parameters$A
  A <- build_companion_A(A_list)
  Ftt <- build_companion_F(Ftt, p)
  max_t <- nrow(Ftt)

  factors_forecast_companion <- (A%^%horizon) %*% Ftt[max_t,]
  factors_forecast <- factors_forecast_companion[1:r]

  forecast <- Lambda %*% factors_forecast
  forecast
}
