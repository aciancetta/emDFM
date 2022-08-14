
<!-- README.md is generated from README.Rmd. Please edit that file -->

# emDFM

<!-- badges: start -->
<!-- badges: end -->

The goal of `emDFM` is to provide an easy tool for estimation of dynamic
factor models. The package allows to estimate the model using PCA, the
Kalman filter or the Expectation Maximization algorithm. Moreover, it
provides useful functions for forecast evaluation and visualization.

## Installation

You can install the development version of emDFM from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("aciancetta/emDFM")
```

## Example

``` r
library(emDFM)
## Download Italian time series from Eurostat and Google
data <- download_clean_data("IT")
d <- data$data_high_freq
scale_fit <- scale_tibble(d)
d_scaled <- scale_fit$scaled_tibble
d_scaled_imputed <- scale_impute(data)

pc_fit <- pca_estimator(d_scaled_imputed, r = 4, p = 1)
param_list <- initialize_filter(pc_fit)
kf_fit <- kalman_filter(d, param_list)
ks_fit <- kalman_smoother(kf_fit)
em_fit <- em_algorithm(d, r = 4, p = 1)


# Plot forecasts
forecast_input <- list(
            estimator = "EM",
            variable = "indpro_SCA",
            horizon = 6,
            r = 4,
            p = 1,
            thresh = 1,
            thresh_imputation = 0.05)

forecast_plot <- forecast_pipeline(d, forecast_input)



# Forecast evaluation
evaluation_input <- list(d = d,
                         horizon = 1,
                         window_size = 376,
                         r = 4,
                         p = 1,
                         thresh_imputation = 0.05,
                         thresh = 0.01
)

param_list <- initialize_filter(pc_fit)
em_eval <- forecast_evaluation_aux(type = "KF", evaluation_input)
plot_forecast_evaluation(em_eval)
```
