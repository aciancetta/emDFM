#' Estimation of the DFM using PCA
#'
#' @description `pca_estimator` takes as input a tibble of time series with the dates
#' in the first column. If there are missing values, these are omitted (even if it is possible to impute them in
#' advance using `scale_impute`).
#' Then the first `r` principal components are extracted and a VAR of order `p` is fitted on them
#' to estimate the autoregressive coefficients of the state equation and the covariance matrix of
#' factors innovations.
#'
#' @param d A tibble of time series data, with dates in the first column
#' @param r The number of factors to extract
#' @param p The autoregressive order of the state equation
#'
#' @return A list of length 4 containing the estimated DFM
#' \item{`factors`}{A matrix containing the estimated factors}
#' \item{`parameters`}{A list containing the parameters of the model.
#' `Lambda` is the matrix of loadings; `A` is the list of the matrices of the autoregressive parameters at each lag;
#' `p` is the autoregressive order of the state equation; `Gamma_eta` is the covariance matrix of the innovations in the state equation}
#' \item{`data_matrix`}{The matrix containing the time series (without dates)}
#' \item{`chi`}{Common component of the time series, obtained as the reconstruction using the first `r` principal components}
#' @export
#'
#' @seealso `scale_impute`
#' @examples
#' # pc_fit <- pca_estimator(d)
#' # pc_fit$factors
#'
pca_estimator <- function(d, r = 4, p = 1){
  d_imputed <- as.matrix(d[,-1])
  ## Fit PCA, get factor_0 and loadings
  is_imputed <- (sum(is.na(d_imputed)) == 0)
  if(is_imputed){
    pca_fit <- stats::prcomp(d_imputed,rank. = r, scale. = FALSE, center = FALSE)
    Lambda <- pca_fit$rotation
    F0 <- pca_fit$x
    chi <- F0 %*% t(Lambda)
  } else {
    warning("Missing values are omitted in order to compute PCA. The missing values in the factors will be omitted.")
    pca_fit <- eigen(stats::cov(d_imputed, use = "complete.obs"))
    Lambda <- pca_fit$vectors[, 1:r]
    F0 <- d_imputed %*% Lambda
    chi <- F0 %*% t(Lambda)
    F0 <- stats::na.omit(F0)
    colnames(F0) <- paste0("F", 1:r)
  }

  var_fit <- vars::VAR(F0, type = "none", p = p)

  eta <- stats::resid(var_fit)
  Gamma_eta <- stats::cov(eta)

  A_list <- vars::Acoef(var_fit)

  return(list(factors = F0,
              parameters = list(Lambda = Lambda,
                                A = A_list,
                                p = p,
                                Gamma_eta = Gamma_eta),
              data_matrix = d_imputed,
              chi = chi ))
}
