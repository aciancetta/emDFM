#' Build the companion matrix of the covariance matrix of the innovations in the state equation
#'
#' @param Gamma A matrix. Covariance matrix of the innovations in the state equation
#' @param p An integer. Autoregressive order of the state equation
#'
#' @return A bolck matrix with the covariance matrix in the first row
#' @export
#'
build_companion_G <- function(Gamma, p){
  r <- ncol(Gamma)
  G <- rbind(diag(1,r), matrix(0, r*(p-1), r))
  G
}
