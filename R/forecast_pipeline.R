#'  Forecast using the Dynamic Factor Model
#'
#' @param dataset A tibble of time series, with dates in the first column
#' @param forecast_input A list
#'
#' @return A plot of the last observed values and the forecasts
#' @export
#'
#' @examples
#' # d <- download_clean_data("IT")
#' # eurostat_indpro_NSA <- get_eurostat("sts_inpr_m",
#' # filters = list(geo = input$geo_code,
#' #               s_adj = "NSA",
#' #               unit = "I15",
#' #               nace_r2 = "C")) %>%
#' #  dplyr::select(date = time, indpro_NSA = values)
#' # seasonal_attributes_indpro <- get_seasonal_attributes_indpro(eurostat_indpro_NSA)
#' # forecast_input <- list(estimator = "EM",
#' #                        variable = "indpro_SCA",
#' #                        horizon = 6,
#' #                        r = 4,
#' #                        p = 1,
#' #                        thresh = 0.01,
#' #                        thresh_imputation = 0.02,
#' #                        seasonal_attributes_indpro = seasonal_attributes_indpro
#' # forecast_plot <- forecast_pipeline(dataset, forecast_input)
#'
#'
forecast_pipeline <- function(dataset, forecast_input){
  r <- forecast_input$r
  thresh_imputation <- forecast_input$thresh_imputation
  thresh <- forecast_input$thresh
  p <- forecast_input$p
  horizon <- forecast_input$horizon
  estimator <- forecast_input$estimator
  variable <- forecast_input$variable
  seasonal_attributes_indpro <- forecast_input$seasonal_attributes_indpro


  scaled_list <- scale_tibble(dataset)
  scale_attributes <- scaled_list$scale_attributes
  d_scaled <- scaled_list$scaled_tibble
  d_imputed <- scale_impute(dataset, r = r, thresh = thresh_imputation)

  if(estimator == "EM"){
    obj_fit <- em_algorithm(d_scaled,
                            thresh_imputation = thresh_imputation,
                            thresh = thresh,
                            show_plots = FALSE)

  } else if (estimator %in% c("PCA", "KF", "KS")){
    obj_fit <- pca_estimator(d = d_imputed, r = r, p = p)
  }
  # } else {
  # stop("Estimator must be either PCA, KF, KS or EM")
  # }

  if (estimator %in% c("KF", "KS")){
    param_list <- initialize_filter(obj_fit)
    obj_fit <- kalman_filter(d_scaled, param_list)
  }

  if(estimator == "KS"){
    obj_fit <- kalman_smoother(obj_fit)
  }

  if(estimator == "package_test"){
    dfm_res <- dynfactoR::dfm(d_scaled[,-1], r = r, p = p)
    obj_fit <- dfm_to_fit(dfm_res)
  }

  plot_forecast(d = d_scaled,
                obj_fit = obj_fit,
                variable = variable, max_horizon = horizon,
                scale_attributes = scale_attributes,
                seasonal_attributes_indpro = seasonal_attributes_indpro
  )
}

