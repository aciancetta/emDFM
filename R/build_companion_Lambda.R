#' Build the companion matrix of the loadings
#'
#' @param Lambda A matrix of loadings
#' @param p An integer. Autoregressive order of the state equation
#'
#' @return A bolck matrix with the loadings in the first row
#' @export

build_companion_Lambda <- function(Lambda, p){
  r <- min(dim(Lambda))
  N <- max(dim(Lambda))
  Lambda_companion <- rbind(t(Lambda), matrix(0, nrow = r*(p-1), ncol = N))
  Lambda_companion
}
