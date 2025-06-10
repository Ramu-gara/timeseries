## Executive Summary
The project utilizes monthly truck sales data from a truck selling company
spanning 2003 to 2014. By applying time series analytics, we forecasted
truck sales for 24 future periods covering 2015 and 2016. The dataset
reveals an upward linear trend combined with multiplicative seasonality—
sales typically increase until mid-year and then decline sharply by the end
of the year. Additionally, statistically significant autocorrelation across all
12 lags indicates strong dependence within the data.

## To forecast truck sales, we implemented four distinct models:

1. A two-level forecasting approach that combines a regression model
with a linear trend and seasonality along with a trailing moving
average.\n
2. Holt-Winter’s exponential smoothing model.
3. A model incorporating a quadratic trend with seasonal adjustments.
4. An Auto ARIMA model.
For each forecasting model, we assessed the performance using both
training/validation splits and the complete dataset. By comparing accuracy
metrics—specifically, RMSE and MAPE—we determined that Holt-
Winter’s model is the optimal choice for forecasting truck sales over the
24 future periods (January 2015 to December 2016).
