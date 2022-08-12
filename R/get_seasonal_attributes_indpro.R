#' Get seasonal attributes for industrial production
#'
#' @description
#' Get the trend, seasonal and residuals components from the tibble of
#' industrial production obtained via eurostat::get_eurostat().
#'
#' @details
#' Please notice that this function is only for initial development and is
#' likely to be deprecated in future releases
#'
#'
#'
#' @param indpro_NSA_tibble A tibble containing a column `date` and a column `indpro_NSA`
#'
#' @return A tibble containing the dates and the trend and seasonal components of industrial production
#' #' @importFrom lubridate %m+% NULL
#'
#' @export
#'
#'
get_seasonal_attributes_indpro <- function(indpro_NSA_tibble){

  `%m+%` <- lubridate::`%m+%`

  first_last_available_idx <- range(which(!is.na(indpro_NSA_tibble$indpro_NSA)))
  indpro_growth_NSA <- diff(indpro_NSA_tibble$indpro_NSA)[first_last_available_idx[1]:first_last_available_idx[2]]
  indpro_growth_NSA <- stats::ts(indpro_growth_NSA, frequency = 12, start = lubridate::ymd(indpro_NSA_tibble$date[first_last_available_idx[1]+1]))

  x13_fit <- RJDemetra::x13(indpro_growth_NSA)

  seas_components_f <- tibble::tibble(date = indpro_NSA_tibble$date[first_last_available_idx[2]] %m+% months(1:12),
                              trend = x13_fit$final$forecasts[,"t_f"],
                              seas = x13_fit$final$forecasts[,"s_f"]) %>%
    dplyr::mutate(is_forecast = TRUE)

  seas_components <- tibble::tibble(date = indpro_NSA_tibble$date[(first_last_available_idx[1]+1):first_last_available_idx[2]],
                            trend = x13_fit$final$series[,"t"],
                            seas = x13_fit$final$series[,"s"]) %>%
    dplyr::mutate(is_forecast = FALSE) %>%
    dplyr::bind_rows(seas_components_f)

  return(seasonal_attributes_indpro = seas_components)

}
