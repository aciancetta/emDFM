#' Convert dynfactoR output to emDFM format
#'
#' @description This function is only for initial development and is likely to
#' get deprecated in future releases.
#'
#' @param dfm_res The output of `dynfactoR::dfm()`
#'
#' @return A list compatible with `emDFM` functions
#' @export
#' @seealso `dynfactoR` package: https://github.com/guilbran/dynfactoR

#' @examples
#' # dfm_res <- dfm(d)
#' # dfm_fit <- dfm_to_fit(dfm_res)
#' # plot_forecast(dfm_fit)
#'
dfm_to_fit <- function(dfm_res){
  r <- ncol(dfm_res$qml)
  p <- dfm_res$p
  A_list <- list()
  for(i in 1:p){
    idx_min <- r*(i-1)+1
    idx_max <- r*i
    A_list[[i]] <- dfm_res$A[, idx_min:idx_max]
  }
  dfm_fit <- list(factors = dfm_res$qml,
                  parameters = list(
                    Lambda = dfm_res$C,
                    p = p,
                    r = r,
                    A = A_list
                  )
  )
  dfm_fit
}
