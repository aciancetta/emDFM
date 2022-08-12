#' Scale data and impute missing values
#'
#' @description Imputation of missing values follows the procedure described in Mc Cracken and Ng (2015).
#' It is based on the EM algorithm in Stock and Watson (2002):
#' - Scale data
#' - Initialize missing values to zero (column mean)
#' - Extract the first `r` principal components
#' - Compute the reconstruction of the dataset based on the `r` principal components
#' - Impute the missing values with their reconstructed values
#' - Repeat PCA and imputation until convergence of the principal components
#'
#'
#' @param d A tibble of time series, having dates as the first column
#' @param r An integer. Number of principal components to use in imputation
#' @param thresh A double defining the threshold for convergence of the algorithm
#'
#' @return A tibble where missing values in `d` have been imputed
#' @export
#'
#' @examples
#' # scale_impute(d)
#'
scale_impute <- function(d, r = 4, thresh = 0.01) {
  n_var <- ncol(d)-1
  d <- outliers_to_missing(d)
  d_scaled <- scale(d[,-1])
  na_idx <- which(is.na(d_scaled))
  d_imputed <- d_scaled
  d_imputed[na_idx] <- 0
  pca_fit <- stats::prcomp(d_imputed, rank. = r)
  loadings <- pca_fit$rotation
  pc <- pca_fit$x
  reconstruction <- pc %*% t(loadings)
  d_imputed[na_idx] <- reconstruction[na_idx]

  # 2) while not converged:
  converged <- FALSE
  while(!converged){
    # 2.a) Extract the principal components
    pc_old <- pc
    pca_fit <- stats::prcomp(d_imputed, rank. = r)
    loadings <- pca_fit$rotation
    pc <- pca_fit$x
    reconstruction <- pc %*% t(loadings)
    d_imputed[na_idx] <- reconstruction[na_idx]

    distance <- norm(pc - pc_old, type = "1")
    message("\rDistance is: ", round(distance/n_var, 3), appendLF = FALSE)
    if(distance/n_var < thresh){
      converged <- TRUE
    }
    mean((d_imputed - pc %*% t(loadings))^2)
  }

  d_imputed_tibble <- d %>%
    tibble::as_tibble() %>%
    dplyr::select(date) %>%
    dplyr::bind_cols(d_imputed)

  d_imputed_tibble
}
