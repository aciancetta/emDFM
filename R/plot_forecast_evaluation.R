#' Plot the results of a forecast evaluation of a DFM
#'
#' @param forecast_eval A list, output of `forecast_evaluation`.
#' @param variable A string defining the target variable.
#' @param title A string with the title to assign to the plot (optional).
#' @param annotate A string with annotations for the plot (optional)
#'
#' @return A plot comparing forecasted values and ground truth.
#' @export
#'
#' @examples
#' # d <- download_clean_data("IT")
#' # eval_obj <- forecast_evaluation(d, horizon = 1, window_size = 300, type = "EM")
#' # plot_forecast_evaluation(eval_obj, variable = "indpro_SCA",
#' #                          title = "Forecast VS ground truth, Italian industrial production")
#'
#'
plot_forecast_evaluation <- function(forecast_eval, variable = "indpro_SCA", title = NULL, annotate = TRUE){

  value <- type <- NULL

  res <- cbind(forecast_eval$ground_truth$date,
               forecast_eval$ground_truth[variable],
               forecast_eval$forecasts[variable])
  names(res) <- c("date", "ground_truth", "forecast")

  res_tidy <- res %>%
    tibble::as_tibble() %>%
    dplyr::mutate(dplyr::across(date,  as.POSIXct)) %>%
    dplyr::mutate(dplyr::across(-date, as.numeric)) %>%
    tidyr::pivot_longer(-date, names_to = "type", values_to = "value")

  plot1 <- res_tidy %>%
    ggplot2::ggplot(ggplot2::aes(x = date, y = value, color = type)) +
    ggplot2::geom_point(size = 2, alpha = 0.8) +
    ggplot2::geom_line(size = 1, alpha = 0.8) +
    ggplot2::geom_hline(yintercept = 0, color = "black", alpha = 0.8, linetype = "dashed") +
    ggplot2::theme_minimal() +
    ggplot2::labs(x = "",
         y = "",
         color = "") +
    ggplot2::scale_x_datetime(breaks = scales::breaks_pretty(6)) +
    ggplot2::scale_color_manual(labels=c("Forecast", "Observed"),
                       values = c("#F8766D", "black")) +
    ggplot2::theme(legend.position = "bottom")

  if(!is.null(title)){
    plot1 <- plot1 +
      ggplot2::labs(title = title)
  }

  if(annotate){
    x_pos <- res_tidy$date[1] + 60*60*24*30*2 # sec*min*hours*days*month
    y_pos <- min(res_tidy$value) #-0.8
    y_range <- max(res_tidy$value) - y_pos
    label_rmsfe <- paste("RMSFE:", round(forecast_eval$forecast_rmse[variable],3))
    label_signs <- paste("Percentage of correct signs:",
                         round(sum(
                           forecast_eval$forecasts[,variable]*forecast_eval$ground_truth[,variable]>0,
                           na.rm = TRUE)*100/nrow(forecast_eval$ground_truth[,variable]),2),
                         "%")

    plot1 <- plot1 +
      ggplot2::annotate("text", x = x_pos, y = y_pos + y_range/12, label = label_rmsfe) +
      ggplot2::annotate("text", x = x_pos, y = y_pos, label = label_signs)
  }
  plot1
}
