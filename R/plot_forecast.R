#' Plot the forecasts of an estimated DFM
#'
#' @param obj_fit A list of estimated parameters of the Dynamic Factor Model, as
#' obtained from `pca_estimator()`, `kalman_filter()`, `kalman_smoother()`, `em_algorithm()`.
#' @param d A tibble of time series, with dates in the first column.
#' @param variable A string. Target variable whose observed and forecasted values are to be drawn.
#' @param max_horizon An integer. Maximum number of steps-ahead of the forecast to plot. Default is 3.
#' @param scale_attributes Scaling attributes of the original dataset, including the mean and standard
#' deviation of each variable in the dataset. Usually obtained from `scale_tibble()$scale_attributes`.
#' @param seasonal_attributes_indpro A tibble of the seasonal attributes of industrial production, as obtained from
#' `get_seasonal_attributes_indpro`. Please notice that this argument is primarily for initial development and is
#' likely to be deprecated in future releases to achieve higher generalization and reproducibility.
#'
#' @return A plot of the observed and forecasted values of the target `variable`.
#' @export
#' @seealso `scale_attributes`
#' @examples
#' # d <- download_clean_data("IT")
#' # scale_fit <- scale_tibble(d)
#' # em_fit <- em_algorithm(d)
#' # eurostat_indpro_NSA <- get_eurostat("sts_inpr_m",
#' # filters = list(geo = input$geo_code,
#' #               s_adj = "NSA",
#' #               unit = "I15",
#' #               nace_r2 = "C")) %>%
#' #  dplyr::select(date = time, indpro_NSA = values)
#' # seasonal_attributes_indpro <- get_seasonal_attributes_indpro(eurostat_indpro_NSA)
#' # plot_forecast(em_fit, d,
#' #               variable = "indpro_SCA",
#' #               max_horizon = 3,
#' #               scale_attributes = scale_fit$scale_attributes,
#' #               seasonal_attributes_indpro = seasonal_attributes_indpro)
#'
#'

plot_forecast <- function(obj_fit,
                          d,
                          variable = "indpro_SCA",
                          max_horizon = 3,
                          scale_attributes = NULL,
                          seasonal_attributes_indpro = NULL){ # to generalize to any series

  `%m+%` <- lubridate::`%m+%`
  is_forecast <- trend <- seas <- NULL

  idx <- which(names(d[,-1]) == variable)

  if(!is.null(scale_attributes)){
    scale_mean <- scale_attributes$center[idx]
    scale_sd <- scale_attributes$scale[idx]
  }

  forecasted_values <- c()
  for(h in 1:max_horizon){
    forecasted_values[h] <- forecast(obj_fit, h)[idx]
  }

  max_t <- nrow(d)
  last_date <- d$date[max_t]
  new_dates <- last_date %m+% months(1:max_horizon)

  d_forecast <- d %>%
    dplyr::select(date, variable = variable) %>%
    dplyr::mutate(is_forecast = FALSE) %>%
    dplyr::slice_tail(n = min(24, max(12, max_horizon*3))) %>%
    dplyr::bind_rows(tibble::tibble(date = new_dates, variable = forecasted_values, is_forecast = TRUE))

  if(!is.null(scale_attributes)){
    d_forecast <- d_forecast %>%
      dplyr::mutate(variable = variable * scale_sd + scale_mean)
  }

  if(!is.null(seasonal_attributes_indpro)){
    d_forecast <- d_forecast %>%
      dplyr::left_join(seasonal_attributes_indpro) %>%
      dplyr::mutate(variable = variable + trend + seas) %>%
      dplyr::select(date, variable, is_forecast)
  }


  d_forecast %>%
    ggplot2::ggplot(ggplot2::aes(x = date, y = variable, color = is_forecast, group = 1)) +
    ggplot2::geom_point(size = 2, alpha = 0.8) +
    ggplot2::geom_line(size = 1, alpha = 0.8) +
    ggplot2::geom_hline(yintercept = 0, color = "black", alpha = 0.8, linetype = "dashed") +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = paste0(max_horizon,"-step ahead forecast of industrial production"),#,variable),
         x = "",
         y = "",
         color = "") +
    ggplot2::scale_color_manual(labels=c("Observed", "Forecast"),
                       values = c("black", "#F8766D")) +
    ggplot2::theme(legend.position = "bottom")
}
