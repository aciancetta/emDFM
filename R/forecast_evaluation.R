#' Perform rolling-window out-of-sample forecast evaluation of the DFM
#' @description
#' `forecast_evaluation` automatizes the process of forecast evaluation of the DFM.
#' It takes in input a tibble of time series and automatically performs a rolling-window
#' out-of-sample forecast evaluation of the model using the chosen estimation algorithm.
#'
#'
#'
#' @param d A tibble of time series, with dates in the first column.
#' @param r An integer. Number of factors to estimate.
#' @param horizon An integer. Number of steps-ahead of the forecast. Default is 1.
#' @param window_size An integer. Number of observations in the sliding window at each step.
#' @param p An integer. Autoregressive order of the state equation.
#' @param thresh A double. Threshold for convergence of the EM algorithm.
#' @param thresh_imputation A double. Threshold for the imputation of missing values (for algorithm initialization).
#' @param type A string defining the algorithm for estimation. It must be either "EM", "PCA", "KF", "KS". Also, for
#' initial development, a "package_test" option is available to compare the results with those from the `dynfactoR`
#' package. This option will be likely be deprecated in future releases.
#'
#' @return A list. The first element is a tibble containing, for each element in the test set,
#' the forecasted value, the ground truth and the forecast error. The second element contains the RMSE for each
#' variable in the dataset
#' @export
#' @seealso `dynfactoR` package: https://github.com/guilbran/dynfactoR
#'
#' @examples
#' # d <- download_clean_data("IT")
#' # forecast_evaluation(d, horizon = 1, window_size = 300, type = "EM")
#'
#'
#'
forecast_evaluation <- function(d,
                                horizon = 1,
                                window_size = 376,
                                r = 4, p = 1,
                                thresh = 0.01, thresh_imputation = 0.05,
                                type = c("EM", "PCA", "KF", "KS", "package_test")){

  max_t <- nrow(d)
  max_iter <- max_t - window_size - horizon + 1

  if(type == "EM"){

    algorithm <- function(d,r,p){
      d_scaled <- cbind(d[,1], scale(d[,-1])) %>% tibble::as_tibble() # scale only the train set, don't use mean/variance from future values
      em_algorithm(d_scaled,r,p, thresh = thresh, thresh_imputation = thresh_imputation)
    }

  } else if(type == "PCA"){

    algorithm <- function(d,r,p){
      d_scaled <- scale_impute(d, thresh = thresh_imputation)
      pca_estimator(d_scaled,r,p)
    }

  } else if (type == "KF"){
    algorithm <- function(d,r,p){
      d_scaled_imputed <- scale_impute(d, thresh = thresh_imputation)
      pc_fit <- pca_estimator(d = d_scaled_imputed, r=r, p=p)
      param_list <- initialize_filter(pc_fit)
      d_scaled <- cbind(d[,1], scale(d[,-1])) %>% tibble::as_tibble()
      kalman_filter(d_scaled, parameters = param_list)
    }

  } else if(type == "KS"){

    algorithm <- function(d,r,p){
      d_scaled_imputed <- scale_impute(d, thresh = thresh_imputation)
      pc_fit <- pca_estimator(d = d_scaled_imputed,
                              r=r, p=p)
      param_list <- initialize_filter(pc_fit)
      d_scaled <- cbind(d[,1], scale(d[,-1])) %>% tibble::as_tibble()
      kf_fit <- kalman_filter(d_scaled, parameters = param_list)
      kalman_smoother(kf_fit)
    }

  } else if(type == "package_test"){

    algorithm <- function(d, r, p){
      dfm_res <- dynfactoR::dfm(X = d[,-1], r=r, p=p)
      dfm_fit <- dfm_to_fit(dfm_res)
      dfm_fit
    }

  } else {
    stop("'type' must be EM, PCA, KF or KS")
  }

  forecast_list <- list()
  ground_truth_list <- list()
  forecast_error_list <- list()
  reference_dates <- c()
  var_names <- names(d)
  for(i in 1:max_iter){
    cat("\n")

    # Drop constant columns from train set
    variances <- lapply(d[i:(i+window_size-1),], stats::var, na.rm = TRUE)
    idx_variables <- which(!is.na(variances) & variances>0)
    d_temp <- d[,idx_variables]
    message("\rRolling ", i, " of ", max_iter,
            ". Using ", ncol(d_temp)-1, " variables", appendLF = TRUE)

    train_set <- d_temp[i:(i+window_size-1),]
    test_set <- unlist(d[i+window_size-1 + horizon, -1]) # exclude dates
    date <- unlist(d_temp[i+window_size-1 + horizon, 1]) # get date

    obj_fit <- algorithm(d = train_set, r=r, p=p)
    obj_forecast <- forecast(obj_fit, horizon = horizon)
    complete_forecast <- rep(NA, (ncol(d)-1))
    for(j in 2:length(idx_variables)){
      original_idx <- idx_variables[j]
      complete_forecast[original_idx-1] <- obj_forecast[j] # account for date
    }

    forecast_error <- complete_forecast - test_set
    forecast_list[[i]] <- complete_forecast
    ground_truth_list[[i]] <- test_set
    forecast_error_list[[i]] <- forecast_error
    reference_dates <- c(reference_dates, date)
  }
  results <- list(forecasts = forecast_list,
                  ground_truth = ground_truth_list,
                  forecast_errors = forecast_error_list)
  results_tidy <- lapply(results,
                         function(x){
                           d_temp1 <- t(as.data.frame(x));
                           d_temp1 <- cbind(reference_dates, d_temp1);
                           res <- tibble::as_tibble(d_temp1);
                           names(res) <- var_names;
                           res %>%
                             dplyr::mutate(dplyr::across(date,  as.Date)) %>%
                             dplyr::mutate(dplyr::across(-date, as.numeric))
                         })

  results_tidy$forecast_rmse <- sqrt(colMeans((results_tidy$forecast_errors[,-1])^2, na.rm = TRUE))

  return(results_tidy)
}

