#' Scale data in a tibble, excluding the first column
#'
#' @param d_tibble A tibble of time series having dates as the first column
#'
#' @return A list containing the scaled tibble as the first element. The second element
#' is a list containing the vector of column means and standard deviations of the
#' original tibble.
#'
#' @export
#'
#' @examples
#' #scale_tibble(d)
#'
scale_tibble <- function(d_tibble){
  x_scaled <- scale(d_tibble[,-1])
  scaled_means <- attr(x_scaled, "scaled:center")
  scaled_sd <- attr(x_scaled, "scaled:scale")

  d_tibble_scaled <- cbind(d_tibble[,1], x_scaled) %>% tibble::as_tibble()

  return(list(scaled_tibble = d_tibble_scaled,
              scale_attributes = list(center = scaled_means,
                                      scale = scaled_sd)))
}
