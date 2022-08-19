#' Wrapper around forecast_evaluation_aux
#'
#' @param type String to choose the algorithm. Either "PCA", "KF", "KS", "EM"
#' @param evaluation_input A list
#'
#' @return See `forecast_evaluation_aux`
#' @export
#' @seealso `forecast_evaluation_aux`
#'
#' @examples
#' # d <- download_clean_data("IT")
#' # evaluation_input <- list(d = d,
#' #                          horizon = 1,
#' #                          window_size = 300
#' #                          r = 4,
#' #                          p = 1,
#' #                          thresh_imputation = 0.2,
#' #                          thresh = 0.01                 # L1 convergence of parameters in EM
#' #                          )
#' #
#' # forecast_eval <- forecast_evaluation_aux("EM", evaluation_input)
#' # plot_forecast_evaluation(forecast_eval)
#'
forecast_evaluation <- function(type, evaluation_input){
  forecast_evaluation_aux(type = type,
                      d = evaluation_input$d,
                      horizon = evaluation_input$horizon,
                      window_size = evaluation_input$window_size,
                      r = evaluation_input$r,
                      p = evaluation_input$p,
                      thresh_imputation = evaluation_input$thresh_imputation,
                      thresh = evaluation_input$thresh)
}
