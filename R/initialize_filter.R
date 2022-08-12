#' Initialize a list of parameters for Kalman filter estimation
#'
#' @param pc_fit A list. The output of `pca_estimator`.
#' @param restrict_Gamma_eta Logical. If TRUE, the covariance matrix of the innovations in the state
#' equation is constrained to be the identity matrix, i.e. the innovations are assumed to be
#' uncorrelated with unit variance. Default is FALSE.
#'
#'
#' @return A list of parameters for Kalman filter initialization:
#' \item{`Lambda`}{Matrix of loadings estimated by `pca_estimator`.}
#' \item{`A`}{List of matrices of the autoregressive parameters in the state equation, estimated via `pca_estimator`.}
#' \item{`Gamma_csi`}{A matrix containing the diagonal of the estimated covariance matrix of the measurement errors.}
#' \item{`Gamma_eta`}{A matrix containing the estimated covariance matrix of the factors' innovations.
#' If `restrict_Gamma_eta` is TRUE, then it is an identity matrix.}
#' \item{`F0`}{A matrix of factors, as estimated by `pca_estimator`.}
#' \item{`p`}{An integer. Autoregressive order of the state equation.}
#' @export
#'
#' @seealso `pca_estimator`
#'
#' @examples
#' # pc_fit <- pca_estimator(d)
#' # param_list <- initializa_filter(pc_fit)
#' # kf_fit <- kalman_filter(d, param_list)
#'
initialize_filter <- function(pc_fit, restrict_Gamma_eta = FALSE) {
  d_imputed <- pc_fit$data_matrix
  F0 <- pc_fit$factors
  Lambda <- pc_fit$parameters$Lambda
  r <- ncol(F0)
  A <- pc_fit$parameters$A
  p <- pc_fit$parameters$p
  chi <- pc_fit$chi
  Gamma_eta <- pc_fit$parameters$Gamma_eta
  if(restrict_Gamma_eta){
    Gamma_eta <- diag(1, r)
  }

  reconstruction_error <- d_imputed - chi       # qua i dati possono non essere stati imputati in precedenza
  Gamma_csi <- diag(diag(stats::cov(reconstruction_error, use = "complete.obs")))
  # Gamma_csi <- cov(reconstruction_error, use = "complete.obs")
  # Gamma_csi[upper.tri(Gamma_csi)] <- 0
  # N <- ncol(d_imputed)
  # n_high_freq <- 30
  # Gamma_csi <- diag(1, N)
  # Gamma_csi[(N-n_high_freq+1):N, (N-n_high_freq+1):N] <- toeplitz(c((-0.5)^(0:(n_high_freq-1))))

  parameters <- list(Lambda = Lambda,
                     A = A,
                     Gamma_csi = Gamma_csi,
                     Gamma_eta = Gamma_eta,
                     F0 = F0,
                     p = p)
  parameters
}

