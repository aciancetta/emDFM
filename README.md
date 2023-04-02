
<!-- README.md is generated from README.Rmd. Please edit that file -->

# emDFM

<!-- badges: start -->
<!-- badges: end -->

The goal of `emDFM` is to provide an easy tool for estimation of the
dynamic factor model:

$$
\begin{aligned}
x_t &= \Lambda f_t + \xi_t \\
f_t &= A_1 f_{t-1} + \dots + A_p f_{t-p} + \eta_t,
\end{aligned}
$$

where $x_t$ is time $t$ observation of the $N$ series and $f_t$ is a
$r$-dimensional factor with $r<<p$.

The package allows to estimate the model using PCA, the Kalman filter or
the Expectation Maximization algorithm. Moreover, it provides useful
functions for forecast evaluation and visualization.

## Installation

You can install the development version of emDFM from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("aciancetta/emDFM")
```

## Example

The dataset must be a tibble of time series, with the first column named
`date` containing the observation dates. The `emDFM` package provides
the `download_clean_data` function, which takes as input the ISO code of
a European country and automatically retrieves its updated macroeconomic
series from Eurostat. Also, it downloads the daily Google Mobility
Indexes referred to that country. The output of the function is a list
of two tibbles, the first containing both the official statistics from
Eurostat and the GMI from Google, the second containing only the
official macroeconomic series. All the series are are made stationary.

``` r
library(emDFM)
## Download Italian time series from Eurostat and Google
data <- download_clean_data("IT")
d <- data$data_high_freq
```

First, we have to scale the data in order to get stable result and make
the results of the estimation scale-independent.

``` r
## Scale the data
scale_fit <- scale_tibble(d)
d_scaled <- scale_fit$scaled_tibble
```

Now, we can move to estimation. The functions `pca_estimator`,
`kalman_filter`, `kalman_smoother` and `em_algorithm` are useful to
comapre the dynamic factors and loadings as estimated by the four
different algorithms. At this stage, we have to choose how many factors
we want to estimate (`r`) and how many lags do we want to use in the
state equation (`p`).

``` r
## PCA
d_scaled_imputed <- scale_impute(d)
pc_fit <- pca_estimator(d_scaled_imputed, r = 4, p = 1)

## Kalman filter
param_list <- initialize_filter(pc_fit)
kf_fit <- kalman_filter(d_scaled, param_list)

## Kalman smoother
ks_fit <- kalman_smoother(kf_fit)
em_fit <- em_algorithm(d_scaled, r = 4, p = 1)
```

The `forecast_pipeline` function allows to easily get the forecasts of
the model estimated using the preferred algorithm and to plot the
results relative to a chosen variable.

``` r
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
```

Finally, the `forecast_evaluation` function allows to evaluate the
forecasting performances of the model using a sliding-window approach.

``` r
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
em_eval <- forecast_evaluation(type = "EM", evaluation_input)
plot_forecast_evaluation(em_eval)
```

# Credits

This package has been developed during my internship at
[IRPET](http://www.irpet.it/) (Regional Institute for Economic Planning
of Tuscany).
