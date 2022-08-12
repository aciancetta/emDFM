#' Substitute outliers with missing values
#'
#' @description
#' Converts outliers in a tibble to NA. Outliers are defined as observations
#' that deviate from the column mean by more than 6 inter-quartile ranges.
#'
#' @param d A tibble of time series containing a column `date`
#'
#' @return A tibble of the same dimension, where outliers has been substituted with NA
#' @export
#'
#' @examples
#' # outliers_to_missing(d)
#'

outliers_to_missing <- function(d){
  d %>%
    dplyr::mutate(dplyr::across(-date, outliers_to_missing_aux))
}
