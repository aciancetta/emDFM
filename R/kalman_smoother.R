#' Kalman smoother estimation of the Dynamic Factor Model
#'
#' @param kalman_filter_fit A list obtained from `kalman_filter()`
#'
#' @return A list `kalman_filter_fit` with factors updated using the Kalman smoother algorithm
#' @export
#'
#' @examples
#' # pc_fit <- pca_estimator(d)
#' # param_list <- initializa_filter(pc_fit)
#' # kf_fit <- kalman_filter(d, param_list)
#' # ks_fit <- kalman_smoother(kf_fit)
#'
kalman_smoother <- function(kalman_filter_fit){

  Ftt <- kalman_filter_fit$factors
  P_list <- kalman_filter_fit$P_list
  F_prediction_list <- kalman_filter_fit$F_prediction_list
  P_prediction_list <- kalman_filter_fit$P_prediction_list
  A <- kalman_filter_fit$parameters$A
  p <- kalman_filter_fit$parameters$p
  r <- ncol(Ftt)

  Ftt <- build_companion_F(Ftt, p)
  A <- build_companion_A(A)

  ## Initialization
  F_ks <- Ftt                #Kalman estimator F_{t|t}
  max_t <- nrow(F_ks)
  ## Recursion ## DA FARE: GESTIRE MISSING VALUES
  for(t in (max_t-1):1){
    ## Prediction equations
    F21_smooth <- F_ks[t+1,]
    F21 <- F_prediction_list[[t+1]]
    P21 <- P_prediction_list[[t+1]]

    F11 <- Ftt[t,]
    P11 <- P_list[[t]]

    ## Update equations
    gain <- P11 %*% t(A) %*% solve(P21)

    # F11_smooth <- F11 + gain %*% (F21_smooth - F21)
    F11_smooth <- F11 + gain %*% t(F21_smooth - F21)

    F_ks[t,] <- F11_smooth
  }

  kalman_filter_fit$factors <- F_ks[,1:r]
  return(kalman_filter_fit)
}
