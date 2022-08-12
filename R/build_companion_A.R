#' Build the companion matrix for the autoregressive parameters in the state equation
#'
#' @param A_list A list containing the matrix of autoregressive parameters for each lag
#'
#' @return A block matrix with all the matrices of `A_list` in the first row
#' @export
#'
#'
build_companion_A <- function(A_list){
  p <- length(A_list)
  r <- ncol(A_list[[1]])
  companion_dim <- p*r

  A_matrix <- do.call(cbind, A_list)
  aux <- cbind(diag(1, companion_dim-r), matrix(0, nrow = companion_dim-r, ncol = r ))

  A_companion <- rbind(A_matrix, aux)
  A_companion
}
