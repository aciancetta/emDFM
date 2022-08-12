#' Perform forecast experiment and compare different algorithms
#'
#' @param evaluation_input A list
#' @param plot_variable String defining the variable to plot
#' @param ggplot_theme ggplot2 theme (optional)
#'
#' @return A list
#' \item{`rmsfe`}{A tibble containing the RMSFE for each variable and for each estimation algorithm.}
#' \item{`plot_forecast`}{Plot of the forecasted vs. observed values for all the estimation algorithm for the selected variable.}
#' \item{`plot_overall_rmsfe`}{Plot of the average RMSFE for each estimation algorithm.}
#' \item{`plot_disagg_rmsfe`}{Plot of the RMSFE for each estimation algorithm for a subset of the variables in the dataset}
#'
#' @export
#'
#' @examples
#' # d <- download_clean_data("IT")
#' # evaluation_input <- list(d = d,
#' #                          horizon = 1,
#' #                          window_size = 300
#' #                          r = 4,
#' #                          p = 1,
#' #                          thresh_imputation = 0.2,
#' #                          thresh = 0.01                 # L1 convergence of parameters in EM
#' #                          )
#' # experiment_result <- forecast_experiment(evaluation_input, plot_variable = "indpro_SCA")
#'
forecast_experiment <- function(evaluation_input,
                                plot_variable = "indpro_SCA",
                                ggplot_theme = NULL){

  estimator <- average_rmsfe <- average_rmse <- variable <- rmse <- algorithm <- NULL
  message("\n\n=======================================")
  message("Starting PCA estimation")
  message("=======================================\n")
  forecast_eval_PCA <- forecast_evaluation_aux(type = "PCA", evaluation_input)

  message("\n\n=======================================")
  message("Starting KF estimation")
  message("=======================================\n")
  forecast_eval_KF <- forecast_evaluation_aux(type = "KF", evaluation_input)

  message("\n\n=======================================")
  message("Starting KS estimation")
  message("=======================================\n")
  forecast_eval_KS <- forecast_evaluation_aux(type = "KS", evaluation_input)

  message("\n\n=======================================")
  message("Starting EM estimation")
  message("=======================================\n")
  forecast_eval_EM <- forecast_evaluation_aux(type = "EM", evaluation_input)

  plot_f_pca_dfm_levels <- plot_forecast_evaluation(forecast_eval_PCA, variable = plot_variable, title = paste0("PCA, ", evaluation_input$horizon, "-step ahead forecast"))
  plot_f_kf_dfm_levels <- plot_forecast_evaluation(forecast_eval_KF, variable = plot_variable, title = paste0("KF, ", evaluation_input$horizon, "-step ahead forecast"))
  plot_f_ks_dfm_levels <- plot_forecast_evaluation(forecast_eval_KS, variable = plot_variable, title = paste0("KS, ", evaluation_input$horizon, "-step ahead forecast"))
  plot_f_em_dfm_levels <- plot_forecast_evaluation(forecast_eval_EM, variable = plot_variable, title = paste0("EM, ", evaluation_input$horizon, "-step ahead forecast"))

  plot_forecast <- egg::ggarrange(plot_f_pca_dfm_levels, plot_f_kf_dfm_levels, plot_f_ks_dfm_levels, plot_f_em_dfm_levels, nrow = 2, ncol = 2)

  overall_rmsfe <- tibble::tibble(pc = mean(forecast_eval_PCA$forecast_rmse, na.rm = TRUE),
                          kf = mean(forecast_eval_KF$forecast_rmse, na.rm = TRUE),
                          ks = mean(forecast_eval_KS$forecast_rmse, na.rm = TRUE),
                          em = mean(forecast_eval_EM$forecast_rmse, na.rm = TRUE)) %>%
    tidyr::pivot_longer(tidyselect::everything(), names_to = "estimator", values_to = "average_rmse")

  plot_overall_rmsfe <- overall_rmsfe %>%
    ggplot2::ggplot(ggplot2::aes(x = stats::reorder(estimator, -average_rmse), y = average_rmse, fill = estimator)) +
    ggplot2::geom_bar(stat = "identity", position = "dodge") +
    ggplot_theme +
    ggplot2::labs(title = "Out-of-sample fit",
         subtitle = "Overall sliding-window RMSFE",
         fill = "")

  disagg_rmsfe <- tibble::tibble(variable = names(forecast_eval_PCA$forecast_rmse),
                         pca = forecast_eval_PCA$forecast_rmse) %>%
    dplyr::bind_cols(kf = forecast_eval_KF$forecast_rmse) %>%
    dplyr::bind_cols(ks = forecast_eval_KS$forecast_rmse) %>%
    dplyr::bind_cols(em = forecast_eval_EM$forecast_rmse) %>%
    dplyr::slice_head(n = 10) %>%
    tidyr::pivot_longer(-variable, names_to = "algorithm", values_to = "rmse")

  plot_disagg_rmsfe <- disagg_rmsfe%>%
    ggplot2::ggplot(ggplot2::aes(x = variable, y = rmse, fill = algorithm)) +
    ggplot2::geom_bar(stat = "identity", position = "dodge") +
    ggplot2::coord_flip() +
    ggplot2::labs(x = "",
         y = "",
         title = "Out-of-sample fit",
         subtitle = "Sliding-window RMSFE disaggregated by variable",
         fill = "") +
    ggplot_theme

  list(rmsfe = overall_rmsfe,
       plot_forecast = plot_forecast,
       plot_overall_rmsfe = plot_overall_rmsfe,
       plot_disagg_rmsfe = plot_disagg_rmsfe)
}
