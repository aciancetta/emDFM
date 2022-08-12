#' Perform in-sample evaluation of the DFM estimated via different algorithms
#'
#' @description
#' `in_sample_evaluation` takes a tibble of time series and compares the in-sample fit of the Dynamic Factor Model
#' using the four different algorithm in `pca_estimator`, `kalman_filter`, `kalman_smoother`, `em_algorithm`.
#' For each estimation method, it reports the values of the RMSE for each variable and plots them.
#'
#' @param d_scaled A tibble of time series, with dates in the first column
#' @param r An integer. Number of factors in the DFM
#' @param p An integer. Autoregressive order of the state equation
#' @param thresh A double. Threshold for convergence of the EM algorithm.
#' @param thresh_imputation A double. Threshold for the imputation of missing values.
#' @param ggplot_theme ggplot2 theme for plots (optional)
#'
#' @return A list:
#' \item{`rmse_in_sample`}{Tibble of in-sample RMSEs for each algorithm and for each variable in the dataset}
#' \item{`plot_overall_rmse`}{Plot of the average RMSE for each algorithm}
#' \item{`plot_disagg_rmse`}{Plot of the RMSEs for each algorithm on a subset of the variables in the dataset}
#' @export
#'
#' @seealso `pca_estimator`, `initialize_filter`, `kalman_filter`, `kalman_smoother`, `em_algorithm`
#'
#' @examples
#'# d <- download_clean_data("IT")
#'# in_sample_evaluation(d)
#'
in_sample_evaluation <- function(d_scaled,
                                 ggplot_theme = NULL,
                                 r = 4, p = 1, thresh_imputation = 0.01,
                                 thresh = 0.01) {

  . <- average_rmse <- estimator <- rmse <- variable <- NULL

  message("\n\n=======================================")
  message("Starting PCA estimation")
  message("=======================================\n")
  d_scaled_imputed <- scale_impute(d_scaled, thresh = thresh_imputation)
  pc_fit <- pca_estimator(d = d_scaled_imputed, r = r, p = p)

  message("\n\n=======================================")
  message("Starting KF estimation")
  message("=======================================\n")
  param_list <- initialize_filter(pc_fit)
  kf_fit <- kalman_filter(d = d_scaled, parameters = param_list)

  message("\n\n=======================================")
  message("Starting KS estimation")
  message("=======================================\n")
  ks_fit <- kalman_smoother(kf_fit)

  message("\n\n=======================================")
  message("Starting EM estimation")
  message("=======================================\n")
  em_fit <- em_algorithm(d = d_scaled, r = r, p = p, thresh = thresh,
                         thresh_imputation = thresh_imputation)

  ## In-sample fit ----
  fits <- list(pc = pc_fit,
               kf = kf_fit,
               ks = ks_fit,
               em = em_fit)

  overall_RMSE <- tibble::tibble(pc = mean(rmse_fit(pc_fit, d = d_scaled)),
                         kf = mean(rmse_fit(kf_fit, d = d_scaled)),
                         ks = mean(rmse_fit(ks_fit, d = d_scaled)),
                         em = mean(rmse_fit(em_fit, d = d_scaled))
  )  %>%
    tidyr::pivot_longer(tidyselect::everything(), names_to = "estimator", values_to = "average_rmse")

  plot_overall_RMSE <- overall_RMSE %>%
    ggplot2::ggplot(ggplot2::aes(x = stats::reorder(estimator, -average_rmse),
               y = average_rmse, fill = estimator)) +
    ggplot2::geom_bar(stat = "identity", position = "dodge") +
    ggplot_theme +
    ggplot2::labs(fill = "",
         title = "In-sample fit",
         subtitle = "Overall RMSE")

  variables <- names(rmse_fit(ks_fit, d = d_scaled))
  disagg_RMSE <- tibble::tibble(pc = rmse_fit(pc_fit, d = d_scaled),
                        kf = rmse_fit(kf_fit, d = d_scaled),
                        ks = rmse_fit(ks_fit, d = d_scaled),
                        em = rmse_fit(em_fit, d = d_scaled)) %>%
    cbind(variable = variables) %>%
    tidyr::pivot_longer(-variable,names_to = "estimator",
                 values_to = "rmse")

  plot_disagg_RMSE <- disagg_RMSE %>%
    .[1:40,] %>%
    ggplot2::ggplot(ggplot2::aes(x = variable, y = rmse, fill = estimator)) +
    ggplot2::geom_bar(stat = "identity", position = "dodge") +
    ggplot2::coord_flip() +
    ggplot_theme +
    ggplot2::labs(title = "In-sample fit",
         subtitle = "RMSE disaggregated by variable",
         fill = "")

  list(rmse_in_sample = overall_RMSE,
       plot_overall_rmse = plot_overall_RMSE,
       plot_disagg_rmse = plot_disagg_RMSE)
}

