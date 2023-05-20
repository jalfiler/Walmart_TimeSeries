### Load the necessary libraries 

library(tidyverse)
library(lubridate)
library(forecast)
library(tseries)

# Load data
walmart_data <- read_csv("walmart_cleaned.csv")

# Convert the date column to a date format
walmart_data$Date <- as.Date(walmart_data$Date, format = "%Y-%m-%d")

# Calculate weekly sales by summing all store sales
weekly_sales <- walmart_data %>%
  group_by(Date) %>%
  summarize(Weekly_Sales = sum(Weekly_Sales, na.rm = TRUE))

# Create a time series object
weekly_sales_ts <- ts(weekly_sales$Weekly_Sales, frequency = 52, start = c(2010, 2))

# Plot the time series
autoplot(weekly_sales_ts)

# Remove trend and seasonality (if needed)
time_series_decomposed <- stl(weekly_sales_ts, s.window="periodic")
autoplot(time_series_decomposed)
time_series_adjusted <- seasadj(time_series_decomposed)

# Apply seasonal differencing
time_series_seasonal_diff <- diff(time_series_adjusted, lag = frequency)
autoplot(time_series_seasonal_diff)

# Apply first differencing if necessary
time_series_diff <- diff(time_series_seasonal_diff)
autoplot(time_series_diff)

# Plot the ACF and PACF
acf(time_series_diff)
pacf(time_series_diff)

# Estimate the models (AR, MA, and ARMA)
model_ar <- arima(time_series_diff, order=c(1,0,0))
model_ma <- arima(time_series_diff, order=c(0,0,1))
model_arma <- arima(time_series_diff, order=c(1,0,1))

# Present the estimation results
summary(model_ar)
summary(model_ma)
summary(model_arma)

# Show the ACF and PACF correlograms of residuals
acf(model_ar$residuals)
pacf(model_ar$residuals)
acf(model_ma$residuals)
pacf(model_ma$residuals)
acf(model_arma$residuals)
pacf(model_arma$residuals)

# Verify residuals are white noise using Q-Test
# 
Box.test(model_ar$residuals, type="Ljung-Box")
Box.test(model_ma$residuals, type="Ljung-Box")
Box.test(model_arma$residuals, type="Ljung-Box")

# Make a six-month ahead forecast
forecast_ar <- forecast(model_ar, h=26)
forecast_ma <- forecast(model_ma, h=26)
forecast_arma <- forecast(model_arma, h=26)

# Plot the multistep of forecasts and their correspondence bands for each specification
autoplot(forecast_ar)
autoplot(forecast_ma)
autoplot(forecast_arma)

