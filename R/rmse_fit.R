#' Compute the in-sample RMSE of a model fit
#'
#' @param obj_fit A list of estimated parameters of the Dynamic Factor Model, as
#' obtained from `pca_estimator()`, `kalman_filter()`, `kalman_smoother()`, `em_algorithm()`
#' @param d A tibble of time series, with dates in the first column
#'
#' @return A vector with the in-sample RMSE of the model for each variable in `d`
#'
#' @seealso `pca_estimator`, `initialize_filter`, `kalman_filter`, `kalman_smoother`, `em_algorithm`
#' @export
#'
#' @examples
#' # d <- download_clean_data("IT")
#' # em_fit <- em_algorithm(d)
#' # rmse_fit(em_fit, d)
#'
rmse_fit <- function(obj_fit, d){
  x <- as.matrix(d[,-1])
  Ftt <- obj_fit$factors
  Lambda <- obj_fit$parameters$Lambda

  colMeans((x-Ftt%*%t(Lambda))^2, na.rm = TRUE)
}
