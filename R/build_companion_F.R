#' Build the companion matrix of factors
#'
#' @param F_matrix A matrix of factors
#' @param p An integer. Autoregressive order of the state equation
#'
#' @return A block matrix with the first p lags of the factors in the first row
#' @export
#'
build_companion_F <- function(F_matrix, p){  # T x (rp) matrix with F_{t-1}, F_{t-2}, ..., F_{t-P} on each row
  r <- ncol(F_matrix)
  t_max <- nrow(F_matrix)
  F_companion <- matrix(0, nrow = t_max, ncol = r*p)
  for(p_current in 1:p){
    i_min <- p-p_current+1                                     # rows to select
    i_max <- t_max - p_current + 1
    j_min <- (p_current-1)*r + 1                               # columns to update
    j_max <- p_current*r
    F_matrix_full <- F_matrix[1:i_max,]
    zeros <- matrix(0, nrow = (p_current-1), ncol = r)
    F_companion[,j_min:j_max] <- rbind(zeros, F_matrix_full)
  }
  F_companion
}
