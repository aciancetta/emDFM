#' Kalman filter estimation of the Dynamic Factor Model
#'
#' @param d A tibble of time series, with dates in the first column
#' @param parameters A list of parameters for filter initialization, obtained with `initialize_filter()`
#'
#' @return A list of parameters:
#' \item{`factors`}{A matrix of estimated factors}
#' \item{`P_list`}{Estimated variance of the forecasts in Kalman filter update}
#' \item{`F_prediction_list`}{Predicted factors in the Kalman filter update}
#' \item{`P_prediction_list`}{Predicted variance of the forecasts in Kalman filter update}
#' \item{`parameters`}{A list containing the parameters of the model.
  #' `Lambda` is the matrix of loadings; `A` is the list of the matrices of the autoregressive parameters at each lag;
  #' `p` is the autoregressive order of the state equation; `Gamma_eta` is the covariance matrix of the innovations in the state equation}
#' @export
#'
#' @examples
#' # pc_fit <- pca_estimator(d)
#' # param_list <- initializa_filter(pc_fit)
#' # kf_fit <- kalman_filter(d, param_list)
#'
kalman_filter <- function(d, parameters){

  x_NAs <- as.matrix(d[,-1]) # data with possibly missing values

  F0 <- parameters$F0
  Lambda <- parameters$Lambda
  A <- parameters$A
  Gamma_csi <- parameters$Gamma_csi
  Gamma_eta <- parameters$Gamma_eta
  p <- parameters$p
  r <- ncol(F0)

  Ftt <- F0                #Kalman estimator F_{t|t}
  Ftt <- build_companion_F(Ftt, p)
  A <- build_companion_A(A)
  G <- build_companion_G(Gamma_eta, p)

  P_list <- list()
  F_prediction_list <- list() # for Kalman smoother
  P_prediction_list <- list() # for Kalman smoother

  ## REVISION
  P11 <- diag(1000, ncol(A))
  # P11 <- matrix(ginv(kronecker(A, A)) %*% as.numeric(G),
  #                 ncol = r * p, nrow = r * p)


  F_prediction_list[[1]] <- Ftt[1,]
  P_prediction_list[[1]] <- P11
  P_list[[1]] <- P11

  Ftt <- matrix(0, nrow(x_NAs), r*p)
  ## Recursion
  for(t in 1:nrow(d)){

    ## Target
    x2 <- x_NAs[t,]

    ## Handling of missing values
    available_idx <- !is.na(x2)
    x2_available <- x2[available_idx]
    Lambda_available <- Lambda[available_idx,]
    Gamma_csi_available <- Gamma_csi[available_idx, available_idx]
    Lambda_available <- build_companion_Lambda(Lambda_available, p)

    ## Prediction equations
    F21 <- F_prediction_list[[t]]
    P21 <- P_prediction_list[[t]]

    ## Update equations
    kalman_gain <- P21 %*% Lambda_available %*% solve(t(Lambda_available) %*% P21 %*% Lambda_available + Gamma_csi_available)
    prediction_error <- x2_available - F21 %*% Lambda_available

    F22 <- F21 + prediction_error %*% t(kalman_gain)
    P22 <- P21 - kalman_gain %*% t(Lambda_available) %*% P21

    ## Store results
    Ftt[t,] <- F22
    P_list[[t]] <- P22
    F_prediction_list[[t+1]] <- F22 %*% t(A)
    P_prediction_list[[t+1]] <- A%*%P22%*%t(A) + G%*%Gamma_eta%*%t(G)
  }

  Ftt <- Ftt[,1:r]

  return(
    list(factors = Ftt,
         P_list = P_list,
         F_prediction_list = F_prediction_list,
         P_prediction_list = P_prediction_list,
         parameters = parameters)
  )
}

