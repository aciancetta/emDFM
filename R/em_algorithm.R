#' Estimate the Dynamic Factor Model using the Expectation Maximization algorithm
#'
#' @param d A tibble of time series, with dates in the first column.
#' @param r An integer. Number of factors to estimate.
#' @param p An integer. Autoregressive order of the state equation.
#' @param thresh A double. Threshold for convergence of the EM algorithm.
#' @param thresh_imputation A double. Threshold for the imputation of missing values (for algorithm initialization).
#' @param max_iter An integer. Maximum number of iterations in the EM algorithm. Default is 30.
#' @param show_plots Logical. Whether to show or not the plot of the factors at each iteration of the EM algorithm.
#' Default is FALSE.
#'
#' @return A list of parameters:
#' \item{`factors`}{A matrix of estimated factors}
#' \item{`P_list`}{List of estimated variance of the forecasts in Kalman filter update}
#' \item{`F_prediction_list`}{List of predicted factors in the Kalman filter update}
#' \item{`P_prediction_list`}{List of predicted variance of the forecasts in Kalman filter update}
#' \item{`parameters`}{A list containing the parameters of the model.
#' `Lambda` is the matrix of loadings; `A` is the list of the matrices of the autoregressive parameters at each lag;
#' `p` is the autoregressive order of the state equation; `Gamma_eta` is the covariance matrix of the innovations in the state equation}
#' @export
#'
#' @examples
#' # d <- download_clean_data()
#' # em_fit <- em_algorithm(d)
#' # em_fit$factors
#'
em_algorithm <- function(d, r = 4, p = 1,
                         thresh = 0.01, thresh_imputation = 0.01,
                         show_plots = FALSE, max_iter = 30) {
  message("Initialization of the missing values for PCA. Distance must reach ",
          thresh_imputation, "\n")

  d_first_imputation <- scale_impute(d, r, thresh_imputation)
  t_max <- nrow(d)
  x <- as.matrix(d[,-1])

  pc_fit <- pca_estimator(d_first_imputation, r, p)
  param_list <- initialize_filter(pc_fit)
  p <- param_list$p

  kf_fit <- kalman_filter(d, parameters = param_list)
  ks_fit <- kalman_smoother(kf_fit)
  theta_update <- unlist(param_list[1:4])

  if(show_plots){
    stats::plot.ts(ks_fit$factors, main = "Kalman smoother factors")
  }

  iter <- 0
  converged <- FALSE
  while(!converged & iter<= max_iter){
    iter <- iter + 1
    theta_current <- theta_update

    P_list <- ks_fit$P_list
    Ftt <- ks_fit$factors
    Ftt <- build_companion_F(Ftt, p)
    # 0) define relevant quantities
    E_FF1 <- 0
    E_F1F1 <- 0
    x_t <- x[1,]
    x_t[is.na(x_t)] <- 0
    E_xF <- x_t %*% t(Ftt[1,])
    E_FF <- Ftt[1,] %*% t(Ftt[1,]) + P_list[[1]]
    E_xx <- x_t %*% t(x_t)
    for(t in 2:t_max){
      x_t <- x[t,]
      x_t[is.na(x_t)] <- 0  # zero update on the components of E_xF and E_xx
      # corresponding to missing values at iteration t
      E_xF <- E_xF + x_t %*% t(Ftt[t,])
      E_FF <- E_FF + Ftt[t,] %*% t(Ftt[t,]) + P_list[[t]]
      E_xx <- E_xx + x_t %*% t(x_t)
      E_FF1 <- E_FF1 + Ftt[t,] %*% t(Ftt[t-1,])
      E_F1F1 <- E_F1F1 + Ftt[t-1,] %*% t(Ftt[t-1,]) + P_list[[t-1]]
    }

    E_FF_a <- E_FF - (Ftt[1,] %*% t(Ftt[1,]) + P_list[[1]])
    E_FF_b <- E_FF - (Ftt[t_max,] %*% t(Ftt[t_max,]) + P_list[[t_max]])

    # 1) update loadings
    Lambda_update <- E_xF %*% solve(E_FF)

    # 2) update transition matrix
    A_update <- E_FF1 %*% solve(E_FF_a)

    # 3) update vcov matrices of measureament and state innovations with
    # estimated covariance of the residuals
    Gamma_csi_update <- diag(diag( E_xx - Lambda_update %*% t(E_xF) ))/(t_max-1)
    Gamma_eta_update <- (E_FF_b - A_update %*% E_FF1)/(t_max-1)


    param_list$Lambda <- Lambda_update[,1:r]
    A_list_update <- list()
    for(i in 1:p){
      A_list_update[[i]] <- A_update[1:r, (r*(i-1)+1):(r*i)]
    }
    param_list$Gamma_csi <- Gamma_csi_update
    param_list$Gamma_eta <- Gamma_eta_update[1:r, 1:r]
    param_list$F0 <- Ftt[,1:r]

    kf_fit <- kalman_filter(d, parameters = param_list)
    ks_fit <- kalman_smoother(kf_fit)

    cat("\nIteration ", iter)
    cat("\nNorm of factors", norm(Ftt))
    cat("\nNorm of factor covariance", norm(crossprod(Ftt)))
    cat("\nNorm of factor-data covariance", norm(crossprod(Ftt, x)))
    cat("\nNorm of loadings", norm(Lambda_update))
    cat("\nNorm of A", norm(A_update))
    cat("\nNorm of Gamma_csi", norm(Gamma_csi_update))
    cat("\nNorm of Gamma_eta", norm(Gamma_eta_update))
    cat("\n")

    if(show_plots){
      plot_title <- paste("EM factors, iteration", iter)
      stats::plot.ts(ks_fit$factors[,1:r], main = plot_title)
    }

    theta_update <- unlist(param_list[1:4])
    distance <- sum(abs(theta_current - theta_update))/length(theta_current) # L1 norm
    message("\rDistance is: ", round(distance, 6), appendLF = TRUE)
    if(distance < thresh){
      converged <- TRUE
    }
  }
  return(
    list(factors = ks_fit$factors,
         parameters = param_list)
  )
}
