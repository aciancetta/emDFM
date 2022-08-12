outliers_to_missing_aux <- function(series, n_iqr = 6){
  sseries <- summary(series)
  iqr <- sseries[5] - sseries[2]
  lower <- sseries[2] - n_iqr*iqr
  upper <- sseries[5] + n_iqr*iqr
  outliers_idx <- which(series < lower | series > upper) ## (Q1 - 3xIQR, Q3 + 3xIQR)
  series[outliers_idx] <- NA
  series
}
