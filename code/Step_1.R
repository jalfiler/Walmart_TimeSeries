#load necessary library

library(tidyverse) #for data manipulation and visualization
library(lubridate) #for datetime data
library(stringr) #for string data
library(forecast) #for time series prediction
library(dplyr)
library(ggplot2)
library(tseries)

#load the data
df <- read.csv('walmart_cleaned.csv')

# View the first few rows of the dataset
head(df)

# Check for missing values in the data frame
sum(is.na(df))

# check types
str(df)


# Convert the "Date" column to a Date format
# Extract day, month, and year components from the Date column
# group by store and date

# Weekly Sales -----------------------------------------------------------------
sales <- df %>%
  mutate(Date = as.Date(Date),
         Day = day(Date),
         Month = month(Date),
         Year = year(Date)) %>% 
  group_by(Store, Date) %>% 
  summarize(Weekly_Sales = sum(Weekly_Sales)) %>% 
  ungroup() %>% 
  filter(Store == '1')

# Create time series object - 52 weeks
time_series <- ts(sales$Weekly_Sales, start = c(2010, 2), frequency = 52)
plot(time_series)


# Plot the graph
ggplot() +
  geom_line(aes(x = time(time_series), y = time_series)) +
  labs(x = "Date", y = "Total weekly sales", title = "Figure 1. Weekly_Sales Time Series from 2010 - 2012")


# Perform the ADF test 
adf.test(time_series)

# Perform the KPSS test 
kpss.test(time_series)


# Create an autocorrelation plot
acf(time_series)

# Create a partial autocorrelation plot
pacf(time_series)

# ---------
# Just out of curiousity although not necessary....

# Perform Differencing for Monthly_Sales
diff_time_series <- diff(time_series,differences = 1, lag = 12)

# Plot the differenced time series
plot(diff_time_series, main = "Differenced Time Series for Monthly_Sales")

# Perform the ADF test after differencing
adf.test(diff_time_series)

# Perform the KPSS test after differencing
kpss.test(diff_time_series)


# Create an autocorrelation plot
acf(diff_time_series)

# Create a partial autocorrelation plot
pacf(diff_time_series)

# # Monthly Sales -----------------------------------------------------------------
# sales <- df %>%
#   mutate(Date = as.Date(Date),
#          Day = day(Date),
#          Month = month(Date),
#          Year = year(Date)) %>% 
#   filter(Date >= as.Date("2010-02-01") & Date <= as.Date("2012-10-31")) %>%
#   group_by(Store, Year, Month) %>% 
#   summarize(Monthly_Sales = sum(Weekly_Sales)) %>% 
#   ungroup() %>% 
#   filter(Store == '1')
# 
# # Create time series object from 2010-2012
# time_series <- ts(sales$Monthly_Sales, start = c(2010, 2), end = c(2012, 10), frequency = 12)
# plot(time_series)
# 
# 
# # Plot the graph
# ggplot() +
#   geom_line(aes(x = time(time_series), y = time_series)) +
#   labs(x = "Date", y = "Total monthly sales", title = "Figure 2. Monthly_Sales Time Series from 2010 - 2012")
# 
# 
# # Perform the ADF test - reject the null hypothesis of non-stationarity and conclude that the time series is stationary.
# adf.test(time_series)
# 
# # Perform the KPSS test -  reject the null hypothesis of level stationarity and conclude that the time series is non-stationary.
# kpss.test(time_series)
# 
# 
# 
# # Perform Differencing for Monthly_Sales
# diff_time_series <- diff(time_series,differences = 1, lag = 12)
# 
# # Plot the differenced time series
# plot(diff_time_series, main = "Differenced Time Series for Monthly_Sales")
# 
# # Perform the ADF test after differencing
# adf.test(diff_time_series)
# 
# # Perform the KPSS test after differencing
# kpss.test(diff_time_series)
# 
# 
# # Create an autocorrelation plot
# acf(diff_time_series)
# 
# # Create a partial autocorrelation plot
# pacf(diff_time_series)

# AR(4)-------------------------------------------------------------------------
# Estimate AR(4) Model using ar() function

# Fit AR(4) model
ar_model <- ar(time_series, order = 4)
ar_model

# Get the residuals
residuals <- resid(ar_model)

# Plot the residuals
plot(residuals, type = "l", main = "Residuals of AR(4) Model")

# Check residuals of AR(4) model
ar_resid <- residuals(ar_model)
acf(ar_resid)
pacf(ar_resid)
Box.test(ar_resid, type = "Ljung-Box")

# Remove rows with missing values
complete_cases <- complete.cases(ar_resid)
ar_resid_complete <- ar_resid[complete_cases]

# Compute ACF and PACF for complete cases
acf(ar_resid_complete)
pacf(ar_resid_complete) # Include plot in the report

# Yule-Walker from Python but ar() in R (coefficients)
ar_coeffs <- ar_model$ar
print(ar_coeffs)



#------------------------
# Fit AR(4) model using ARIMA() function
arima_model <- arima(time_series, order = c(4, 0, 0))

# Print the model summary
summary(arima_model)

# Find AIC and BIC for the model
arima_aic <- AIC(arima_model)
arima_bic <- BIC(arima_model)

# Print AIC and BIC
cat("AR(4) model AIC:", arima_aic, "BIC:", arima_bic, "\n")


#------------------------
# 6-month forecast for AR(4) 
library(forecast)

# Fit ARIMA(4,0,0) model with non-zero mean
arima_model <- arima(time_series, order = c(4, 0, 0), include.mean = TRUE)

# Perform 6-month forecast
forecast_values <- forecast::forecast(arima_model, h = 26, level = c(80, 95))  

# Plot the forecasted values
plot(forecast_values, main = "AR(4) Model 6-month Forecast")

# Looking at MAPE
summary(forecast_values)

# Confirm accuracy with MAPE
accuracy(forecast_values)




# ARMA(4,4) -------------------------------------------------------------------

# Estimate ARMA(4,4) Model using arima() function

# Fit ARMA(4,4) model
arma_model <- arima(time_series, order = c(4, 0, 4))
arma_model

# Print the model summary
summary(arma_model)


# Get the residuals
residuals <- resid(arma_model)

# Plot the residuals
plot(residuals, type = "l", main = "Residuals of ARMA(4,4) Model")

# Check residuals of AR(4) model
arma_resid <- residuals(arma_model)
acf(arma_resid)
pacf(arma_resid)
Box.test(arma_resid, type = "Ljung-Box")

# Remove rows with missing values
complete_cases <- complete.cases(arma_resid)
arma_resid_complete <- arma_resid[complete_cases]

# Compute ACF and PACF for complete cases
acf(arma_resid_complete)
pacf(arma_resid_complete) # Include plot in the report

# Yule-Walker from Python but arima() in R (coefficients)
arma_coeffs <- coef(arma_model)
print(arma_coeffs)


#------------------------
# Fit ARMA(4,4) model using arima() function
arima_model <- arima(time_series, order = c(4, 0, 4))

# Print the model summary
summary(arima_model)

# Find AIC and BIC for the model
arima_aic <- AIC(arima_model)
arima_bic <- BIC(arima_model)

# Print AIC and BIC
cat("ARMA(4,4) model AIC:", arima_aic, "BIC:", arima_bic, "\n")


#------------------------
# 6-month forecast for ARMA(4,4) 
library(forecast)

# Fit ARIMA(4,0,4) model with non-zero mean
arima_model <- arima(time_series, order = c(4, 0, 4), include.mean = TRUE)

# Perform 6-month forecast
forecast_values <- forecast::forecast(arima_model, h = 26, level = c(80, 95))  

# Plot the forecasted values
plot(forecast_values, main = "ARMA(4,4) Model 6-month Forecast")

# Looking at MAPE
summary(forecast_values)

# Confirm accuracy with MAPE
accuracy(forecast_values)






# ARMA(3,4) -------------------------------------------------------------------

# Estimate ARMA(3,4) Model using arima() function

# Fit ARMA(3,4) model
arma_model2 <- arima(time_series, order = c(3, 0, 4))
arma_model2

# Print the model summary
summary(arma_model2)

# Get the residuals
residuals <- resid(arma_model2)

# Plot the residuals
plot(residuals, type = "l", main = "Residuals of ARMA(3,4) Model")

# Check residuals of ARMA(3,4) model
arma_resid2 <- residuals(arma_model2)
acf(arma_resid2)
pacf(arma_resid2)
Box.test(arma_resid, type = "Ljung-Box")

# Yule-Walker from Python but arima() in R (coefficients)
arma_coeffs <- coef(arma_model)
print(arma_coeffs)


#------------------------
# Fit ARMA(3,4) model using arima() function
arima_model <- arima(time_series, order = c(3, 0, 4))

# Print the model summary
summary(arima_model)

# Find AIC and BIC for the model
arima_aic <- AIC(arima_model)
arima_bic <- BIC(arima_model)

# Print AIC and BIC
cat("ARMA(3,4) model AIC:", arima_aic, "BIC:", arima_bic, "\n")


#------------------------
# 6-month forecast for ARMA(3,4) 
library(forecast)

# Fit ARIMA(3,0,4) model with non-zero mean
arima_model <- arima(time_series, order = c(3, 0, 4), include.mean = TRUE)

# Perform 6-month forecast
forecast_values <- forecast::forecast(arima_model, h = 26, level = c(80, 95))  

# Plot the forecasted values
plot(forecast_values, main = "ARMA(3,4) Model 6-month Forecast")

# Looking at MAPE
summary(forecast_values)

# Confirm accuracy with MAPE
accuracy(forecast_values)




# -------------------------



# AR(5) ------------------------------------------------------------------------

# Estimate AR(5) Model using ar() function

# Fit AR(5) model
ar_model <- ar(time_series, order = 5)
ar_model

# Get the residuals
residuals <- resid(ar_model)

# Plot the residuals
plot(residuals, type = "l", main = "Residuals of AR(5) Model")

# Check residuals of AR(4) model
ar_resid <- residuals(ar_model)
acf(ar_resid)
pacf(ar_resid)
Box.test(ar_resid, type = "Ljung-Box")

# Remove rows with missing values
complete_cases <- complete.cases(ar_resid)
ar_resid_complete <- ar_resid[complete_cases]

# Compute ACF and PACF for complete cases
acf(ar_resid_complete)
pacf(ar_resid_complete) # Include plot in the report

# Yule-Walker from Python but ar() in R (coefficients)
ar_coeffs <- ar_model$ar
print(ar_coeffs)


#------------------------
# Fit AR(5) model using ARIMA() function
arima_model <- arima(time_series, order = c(5, 0, 0))

# Print the model summary
summary(arima_model)

# Find AIC and BIC for the model
arima_aic <- AIC(arima_model)
arima_bic <- BIC(arima_model)

# Print AIC and BIC
cat("AR(4) model AIC:", arima_aic, "BIC:", arima_bic, "\n")


#------------------------
# 6-month forecast for AR(5) 
library(forecast)

# Fit AR(5)) model with non-zero mean
arima_model <- arima(time_series, order = c(5, 0, 0), include.mean = TRUE)

# Perform 6-month forecast
forecast_values <- forecast::forecast(arima_model, h = 26, level = c(80, 95))  

# Plot the forecasted values
plot(forecast_values, main = "AR(5) Model 6-month Forecast")

# Looking at MAPE
summary(forecast_values)

# Confirm accuracy with MAPE
accuracy(forecast_values)


# AR(6)------------------------
# # Fit AR(6) model using ARIMA() function
# arima_model <- arima(time_series, order = c(6, 0, 0))
# 
# # Print the model summary
# summary(arima_model)
# 
# # Plot the residuals
# residuals <- resid(arima_model)
# plot(residuals, type = "l", main = "Residuals of AR(6) Model")
# 
# ar_resid <- residuals(arima_model)
# acf(arima_resid)
# pacf(arima_resid)
# Box.test(ar_resid, type = "Ljung-Box")
# 
# # Remove rows with missing values
# complete_cases <- complete.cases(ar_resid)
# ar_resid_complete <- ar_resid[complete_cases]
# 
# # Compute ACF and PACF for complete cases
# acf(ar_resid_complete)
# pacf(ar_resid_complete) # Include plot in the report
# 
# # Find AIC and BIC for the model
# arima_aic <- AIC(arima_model)
# arima_bic <- BIC(arima_model)
# 
# # Print AIC and BIC
# cat("AR(4) model AIC:", arima_aic, "BIC:", arima_bic, "\n")

