######### GROUP 5   #############
# DTC FINAL CODE 


# Upload the necessary libraries that is used for this analysis 

library(tidyverse) #for data manipulation and visualization
library(lubridate) #for datetime data
library(stringr) #for string data
library(forecast) #for time series prediction
library(dplyr)
library(ggplot2)
library(tseries)
library(knitr)

#load the data
df <- read.csv('walmart_cleaned.csv')

# View the first few rows of the dataset
head(df)

# Check for missing values in the data frame
sum(is.na(df))

# check data types of variables
str(df)

# Convert the "Date" column to a Date format
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
# Result supports stationarity - supports the alternate hypothesis (stationary)

# Perform the KPSS test 
kpss.test(time_series)

# Perform Differencing for Weekly_Sales
diff_time_series <- diff(time_series,differences = 1, lag = 12) # seasonal differencing

# Plot the differenced time series
plot(diff_time_series, main = "Differenced Time Series for Weekly_Sales")

# Perform the ADF test after differencing
adf.test(diff_time_series)
# Perform the KPSS test after differencing
kpss.test(diff_time_series)

# Create an autocorrelation plot
acf(diff_time_series)

# Create a partial autocorrelation plot
pacf(diff_time_series)

# Create identified models and summary table for the models 

# Initialize an empty dataframe to hold model summary tables
summary_tables <- data.frame(Model=character(),
                             Q_stat=numeric(),
                             P_value=numeric(),
                             Residual_Variance=numeric(),
                             R_squared=numeric(),
                             Adjusted_R_squared=numeric(),
                             AIC=numeric(),
                             BIC=numeric(),
                             stringsAsFactors=FALSE)

create_summary_table <- function(model, model_name, residuals, Y) {
  R2 = cor(fitted(model), Y)^2
  n = length(Y)
  RA2 = 1-(1-R2)*(n-1)/(n-2)
  df <- data.frame(
    Model = model_name,
    Q_stat = Box.test(residuals, type = "Ljung-Box")$statistic,
    P_value = Box.test(residuals, type = "Ljung-Box")$p.value,
    Residual_Variance = var(residuals),
    R_squared = R2,
    Adjusted_R_squared = RA2,
    AIC = AIC(model),
    BIC = BIC(model),
    row.names = NULL
  )
  return(df)
}

# AR(3) model_____________________________________________
ar3_model <- arima(time_series, order = c(3, 0, 0), include.mean = TRUE)

# Get the residuals
ar3_residuals <- resid(ar3_model)

# Plot the residuals
plot(ar3_residuals, type = "l", main = "Residuals of AR(3) Model")

# Check residuals of AR(3) model
acf(ar3_residuals) 
pacf(ar3_residuals) 
Box.test(ar3_residuals, type = "Ljung-Box")

# Yule-Walker from Python but ar() in R (coefficients)
ar3_coeffs <- ar3_model$ar
print(ar3_coeffs)

# Find AIC and BIC for the model
arima_aic <- AIC(ar3_model)
arima_bic <- BIC(ar3_model)

# Print AIC and BIC
cat("AR(3) model AIC:", arima_aic, "BIC:", arima_bic, "\n")

# Perform 6-month forecast
forecast_values <- forecast::forecast(ar3_model, h = 26, level = c(80, 95))  

# Plot the forecasted values
plot(forecast_values, main = "AR(3) Model 6-month Forecast")

# Looking at MAPE
summary(forecast_values)

# Confirm accuracy with MAPE
accuracy(forecast_values)

# Add the summary table for the AR(3) model
summary_tables <- rbind(summary_tables, create_summary_table(ar3_model, 'AR(3)', ar3_residuals, time_series))



# AR(4) model_____________________________________________
ar4_model <- arima(time_series, order = c(4, 0, 0), include.mean = TRUE)

# Get the residuals
ar4_residuals <- resid(ar4_model)

# Plot the residuals
plot(ar4_residuals, type = "l", main = "Residuals of AR(4) Model")

# Check residuals of AR(4) model
acf(ar4_residuals) 
pacf(ar4_residuals) 
Box.test(ar4_residuals, type = "Ljung-Box")

# Yule-Walker from Python but ar() in R (coefficients)
ar4_coeffs <- ar4_model$ar
print(ar4_coeffs)

# Find AIC and BIC for the model
arima_aic <- AIC(ar4_model)
arima_bic <- BIC(ar4_model)

# Print AIC and BIC
cat("AR(4) model AIC:", arima_aic, "BIC:", arima_bic, "\n")

# Perform 6-month forecast
forecast_values <- forecast::forecast(ar4_model, h = 26, level = c(80, 95))  

# Plot the forecasted values
plot(forecast_values, main = "AR(4) Model 6-month Forecast")

# Looking at MAPE
summary(forecast_values)

# Confirm accuracy with MAPE
accuracy(forecast_values)

# Add the summary table for the AR(4) model
summary_tables <- rbind(summary_tables, create_summary_table(ar4_model, 'AR(4)', ar4_residuals, time_series))

# AR(5) model_____________________________________________
ar5_model <- arima(time_series, order = c(5, 0, 0), include.mean = TRUE)

# Get the residuals
ar5_residuals <- resid(ar5_model)

# Plot the residuals
plot(ar5_residuals, type = "l", main = "Residuals of AR(5) Model")

# Check residuals of AR(5) model
acf(ar5_residuals) 
pacf(ar5_residuals) 
Box.test(ar5_residuals, type = "Ljung-Box")

# Yule-Walker from Python but ar() in R (coefficients)
ar5_coeffs <- ar5_model$ar
print(ar5_coeffs)

# Find AIC and BIC for the model
arima_aic <- AIC(ar5_model)
arima_bic <- BIC(ar5_model)

# Print AIC and BIC
cat("AR(5) model AIC:", arima_aic, "BIC:", arima_bic, "\n")

# Perform 6-month forecast
forecast_values <- forecast::forecast(ar5_model, h = 26, level = c(80, 95))  

# Plot the forecasted values
plot(forecast_values, main = "AR(5) Model 6-month Forecast")

# Looking at MAPE
summary(forecast_values)

# Confirm accuracy with MAPE
accuracy(forecast_values)

# Add the summary table for the AR(5) model
summary_tables <- rbind(summary_tables, create_summary_table(ar4_model, 'AR(5)', ar5_residuals, time_series))


# ARMA(4,4) -------------------------------------------------------------------

# Fit ARMA(4,4) model
arma_44_model <- arima(time_series, order = c(4, 0, 4))
arma_44_model
summary(arma_44_model)

# Get the residuals
residuals_arma_44 <- resid(arma_44_model) #once

# Plot the residuals
plot(residuals_arma_44, type = "l", main = "Residuals of ARMA(4,4) Model")

# Check residuals of AR(4) model
acf(residuals_arma_44)
pacf(residuals_arma_44)
Box.test(residuals_arma_44, type = "Ljung-Box")

# Yule-Walker from Python but arima() in R (coefficients)
arma_44_coeffs <- coef(arma_44_model)
print(arma_44_coeffs)

# Find AIC and BIC for the model
arima_aic <- AIC(arma_44_model)
arima_bic <- BIC(arma_44_model)

# Print AIC and BIC
cat("ARMA(4,4) model AIC:", arima_aic, "BIC:", arima_bic, "\n")

# Perform 6-month forecast
forecast_values <- forecast::forecast(arma_44_model, h = 26, level = c(80, 95))  

# Plot the forecasted values
plot(forecast_values, main = "ARMA(4,4) Model 6-month Forecast")

# Looking at MAPE
summary(forecast_values)

# Confirm accuracy with MAPE
accuracy(forecast_values)

# Add the summary table for the ARMA(44) model
summary_tables <- rbind(summary_tables, create_summary_table(arma_44_model, 'ARMA(4,4)', residuals_arma_44, time_series))


# ARMA(3,4) -------------------------------------------------------------------

# Fit ARMA(3,4) model
arma_34_model <- arima(time_series, order = c(3, 0, 4))
arma_34_model
summary(arma_34_model)

# Get the residuals
residuals_arma_34 <- resid(arma_34_model)

# Plot the residuals
plot(residuals_arma_34, type = "l", main = "Residuals of ARMA(3,4) Model")

acf(residuals_arma_34)
pacf(residuals_arma_34)
Box.test(residuals_arma_34, type = "Ljung-Box")

# Yule-Walker from Python but arima() in R (coefficients)
arma_34_coeffs <- coef(arma_34_model)
print(arma_34_coeffs)

# Find AIC and BIC for the model
arima_aic <- AIC(arma_34_model)
arima_bic <- BIC(arma_34_model)

# Print AIC and BIC
cat("ARMA(3,4) model AIC:", arima_aic, "BIC:", arima_bic, "\n")

# Perform 6-month forecast
forecast_values <- forecast::forecast(arma_34_model, h = 26, level = c(80, 95))  

# Plot the forecasted values
plot(forecast_values, main = "ARMA(3,4) Model 6-month Forecast")

# Looking at MAPE
summary(forecast_values)

# Confirm accuracy with MAPE
accuracy(forecast_values)

# Add the summary table for the ARMA(44) model
summary_tables <- rbind(summary_tables, create_summary_table(arma_34_model, 'ARMA(3,4)', residuals_arma_34, time_series))


#Look at combined summary table
kable(summary_tables, digits = 3)




# STEP 2 OUT OF SAMPLE EVALUATION

cutoff_index <- round(0.9 * length(time_series))

#Fitting the models 

# AR(5) model
AR5_model <- arima(time_series[1:cutoff_index], order=c(5,0,0))
summary(AR5_model)

# ARMA(3,4) model
ARMA34_model <- arima(time_series[1:cutoff_index], order=c(3,0,4))
summary(ARMA34_model)

# ARMA(4,4) model
ARMA44_model <- arima(time_series[1:cutoff_index], order=c(4,0,4))
summary(ARMA44_model)

#Initialize empty vectors to store forecasted values, forecast errors, and squared losses
n_prediction <- length(time_series) - cutoff_index

# Forecasted values
fcastAR5 <- fcast34 <- fcast44 <-fcastNaive <- numeric(n_prediction)

# forecast errors
ferrorAR5 <- ferror34 <- ferror44 <- ferrorNaive <- numeric(n_prediction)

# squared losses
lossAR5 <- loss34 <- loss44 <- lossNaive <-numeric(n_prediction)

# one-step ahead forecast (h=1) means that you are predicting the next value in the time series based on the current available data
# A loop to generate forecasted values, forecast errors, and squared losses for all four models:
for (i in 0:(n_prediction-1)) {
  
  # Update the models with the newly observed data point
  AR5_model <- arima(time_series[1:(cutoff_index+i)], order=c(5,0,0))
  ARMA34_model <- arima(time_series[1:(cutoff_index+i)], order=c(3,0,4))
  ARMA44_model <- arima(time_series[1:(cutoff_index+i)], order=c(4,0,4))
  
  # Simple average of last 4 observations
  fcastNaive[i+1] <- mean(time_series[(i+cutoff_index-3):(i+cutoff_index)])
  ferrorNaive[i+1] <- time_series[i + cutoff_index + 1] - fcastNaive[i+1]
  lossNaive[i+1] <- ferrorNaive[i+1]^2
  
  # ARMA(3,4) model
  fcast34[i+1] <- predict(ARMA34_model, n.ahead = 1)$pred
  ferror34[i+1] <- time_series[i + cutoff_index + 1] - fcast34[i+1]
  loss34[i+1] <- ferror34[i+1]^2
  
  # ARMA(4,4) model
  fcast44[i+1] <- predict(ARMA44_model, n.ahead = 1)$pred
  ferror44[i+1] <- time_series[i + cutoff_index + 1] - fcast44[i+1]
  loss44[i+1] <- ferror44[i+1]^2
  
  # AR(5) model
  fcastAR5[i+1] <- predict(AR5_model, n.ahead = 1)$pred
  ferrorAR5[i+1] <- time_series[i + cutoff_index + 1] - fcastAR5[i+1]
  lossAR5[i+1] <- ferrorAR5[i+1]^2
}

#Combining the forecasted values, forecast errors, and squared losses into a single dataframe:
resultsAR5 <- data.frame(fcast = fcastAR5, ferror = ferrorAR5, loss = lossAR5)
results34 <- data.frame(fcast = fcast34, ferror = ferror34, loss = loss34)
results44 <- data.frame(fcast = fcast44, ferror = ferror44, loss = loss44)
resultsNaive <- data.frame(fcast = fcastNaive, ferror = ferrorNaive, loss = lossNaive)

# Combine all results into a single data frame:
all_results <- rbind(cbind(model = "Last 4 Obs. Avg.", resultsNaive),
                     cbind(model = "ARMA(3,4)", results34),
                     cbind(model = "ARMA(4,4)", results44),
                     cbind(model = "AR(5)", resultsAR5)
)

# Calculate MSE for each model
#average squared difference between the estimated values and the actual value. 
#A model with a smaller MSE is generally considered better.
MSENaive <- mean(lossNaive)
MSEAR5 <- mean(lossAR5)
MSE34 <- mean(loss34)
MSE44 <- mean(loss44)

# Create a dataframe with MSE values
mse_df <- data.frame(Model = c("Last 4 Obs. Avg." , "ARMA(3,4)" , "ARMA(4,4)", "AR(5)"),
                     MSE = c(MSENaive, MSE34, MSE44, MSEAR5))
print(mse_df)


# Calculate MAE for each model to test unconditional predictability 

MAEAR5 <- mean(abs(ferrorAR5))
MAE34 <- mean(abs(ferror34))
MAE44 <- mean(abs(ferror44))
MAENaive <- mean(abs(ferrorNaive))

# Create a dataframe with MAE values
mae_df <- data.frame(Model = c("Last 4 Obs. Avg." , "ARMA(3,4)" , "ARMA(4,4)", "AR(5)"),
                     MAE = c(MAENaive, MAE34, MAE44, MAEAR5))
print(mae_df)

# Perform MPE and IE tests for each model

# Simple average of last 4 observations
mpetestNaive <- lm(ferrorNaive ~ 1)
summary(mpetestNaive)

# ARMA(3,4) model
mpetest34 <- lm(ferror34 ~ 1)
summary(mpetest34)

# ARMA(4,4) model
mpetest44 <- lm(ferror44 ~ 1)
summary(mpetest44)

# AR(5) model
mpetestAR5 <- lm(ferrorAR5 ~ 1)
summary(mpetestAR5)

# Extracting the coefficient values from the MPE tests
mpe_values <- data.frame(
  Model = c("Simple Average", "ARMA(3,4)", "ARMA(4,4)", "AR(5)"),
  
  Estimate = c(coef(summary(mpetestNaive))[, 1],
               coef(summary(mpetest34))[, 1], 
               coef(summary(mpetest44))[, 1],
               coef(summary(mpetestAR5))[, 1]))
  
print(mpe_values)


# Simple average of last 4 observations
IETestNaive <- lm(ferrorNaive ~ fcastNaive)
summary(IETestNaive)

# ARMA(3,4) model
IETest34 <- lm(ferror34 ~ fcast34)
summary(IETest34)

# ARMA(4,4) model
IETest44 <- lm(ferror44 ~ fcast44)
summary(IETest44)

# AR(5) model
IETestAR5 <- lm(ferrorAR5 ~ fcastAR5)
summary(IETestAR5)

# Getting the important values for the IE test.
iet_values <- data.frame(
  Model = c("Simple Average", "ARMA(3,4)", "ARMA(4,4)", "AR(5)"),
  Estimate = c(coef(summary(IETestNaive))[2, 1], coef(summary(IETest34))[2, 1],
               coef(summary(IETest44))[2, 1], coef(summary(IETestAR5))[2, 1]),
  
  Std.Error = c(coef(summary(IETestNaive))[2, 2], coef(summary(IETest34))[2, 2],
                coef(summary(IETest44))[2, 2], coef(summary(IETestAR5))[2, 2]),
  
  t_value = c(coef(summary(IETestNaive))[2, 3], coef(summary(IETest34))[2, 3],
              coef(summary(IETest44))[2, 3], coef(summary(IETestAR5))[2, 3]),
  
  p_value = c(coef(summary(IETestNaive))[2, 4], coef(summary(IETest34))[2, 4],
              coef(summary(IETest44))[2, 4], coef(summary(IETestAR5))[2, 4])
)
print(iet_values)


## COMBINED FORECAST ##

# Use three linear combination schemes: 
# 1) an equal-weighted forecast,
# 2) a forecast that weights each individual forecast by the inverse of its MSE, 
# 3) an OLS weighted

combined_forecast_equal <- numeric(n_prediction)
combined_forecast_inverse <- numeric(n_prediction)
combined_forecast_ols <- numeric(n_prediction)

# Equal weighted optimal forecast
weight_equal <- 1 / 4 

# weighted sum
combined_forecast_equal <- weight_equal * fcastNaive + weight_equal * fcastAR5 + weight_equal * fcast34 + weight_equal * fcast44

# Implement the inverse-MSE weighted combination scheme:
# Inverse of MSE as weights
tot_MSE = 1/MSEAR5 + 1/MSE34 + 1/MSE44 + 1/MSENaive
inverse_w1 = (1/MSEAR5) / tot_MSE
inverse_w2 = (1/MSE34) / tot_MSE
inverse_w3 = (1/MSE44) / tot_MSE
inverse_w4 = (1/MSENaive) / tot_MSE

# weighted sum
combined_forecast_inverse<- inverse_w1 * fcastAR5 + inverse_w2 * fcast34 + inverse_w3 * fcast44 + inverse_w4 * fcastNaive

# OLS weighted combination scheme
# Fit an OLS regression model using the individual forecast values as predictors
# Subset of the time_series vector starting from the index cutoff_index + 1 to the end of the vector. 

ols_model <- lm(time_series[(cutoff_index + 1):length(time_series)] ~ fcastAR5 + fcast34 + fcast44 + fcastNaive )

# Extract the coefficients (weights) from the OLS model
weights_ols <- coef(ols_model)[-1]

# Calculate the combined forecast using the OLS weights
combined_forecast_ols <- cbind(fcastAR5, fcast34, fcast44, fcastNaive) %*% weights_ols

# Calculate errors for each scheme
error_equal <- time_series[(cutoff_index + 1):length(time_series)] - combined_forecast_equal
error_inverse <- time_series[(cutoff_index + 1):length(time_series)] - combined_forecast_inverse
error_ols <- time_series[(cutoff_index + 1):length(time_series)] - combined_forecast_ols

# Calculate losses for each scheme
loss_equal <- error_equal^2
loss_inverse <- error_inverse^2
loss_ols <- error_ols^2

# Calculate MSE for each scheme
mse_equal <- mean(loss_equal)
mse_inverse <- mean(loss_inverse)
mse_ols <- mean(loss_ols)


# Combine all the values into a table
combined_table <- data.frame(
  Actual = time_series[(cutoff_index + 1):length(time_series)],
  Forecast_Naive = fcastNaive,
  Forecast_AR5 = fcastAR5,
  Forecast_34 = fcast34,
  Forecast_44 = fcast44,
  Combined_Equal = combined_forecast_equal,
  Combined_Inverse = combined_forecast_inverse,
  Combined_OLS = combined_forecast_ols,
  MSE_Equal = loss_equal,
  MSE_Inverse = loss_inverse,
  MSE_OLS = loss_ols
)
print(combined_table)

#Table 9.8 on page 246
weights_equal <- rep(weight_equal, 4)
weights_inverse <- c(inverse_w1, inverse_w2, inverse_w3, inverse_w4)
weights_ols <- weights_ols

weights_table <- data.frame(
  Scheme = c("Equal", "Inverse MSE", "OLS"),
  AR5 = c(weight_equal, inverse_w1, weights_ols[1]),
  ARMA34 = c(weight_equal, inverse_w2, weights_ols[2]),
  ARMA44 = c(weight_equal, inverse_w3, weights_ols[3]),
  Naive = c(weight_equal, inverse_w4, weights_ols[4])
)

print(weights_table)

# MSE values for each scheme
mse_table_combined <- data.frame(
  Scheme = c("Equal Weighted", "Inverse MSE Weighted", "OLS Weighted"),
  MSE = c(mse_equal, mse_inverse, mse_ols)
)

print(mse_table_combined)


# PLOTS

# Create a data frame with predicted values and actual values 

forecast_data <- data.frame(
  Date = sales$Date[(cutoff_index + 1):(cutoff_index + n_prediction)],
  Actual = sales$Weekly_Sales[(cutoff_index + 1):(cutoff_index + n_prediction)],
  ARMA34 = fcast34,
  ARMA44 = fcast44,
  AR5 = fcastAR5,
  Naive = fcastNaive,
  Combined_Equal = combined_forecast_equal,
  Combined_Inverse = combined_forecast_inverse,
  Combined_OLS = combined_forecast_ols
)

# Create a plot of the predicted values and actual values
# Create the time series plot

ggplot(forecast_data, aes(x = Date)) +
  geom_line(aes(y = Actual, color = "Actual"), linetype = "dashed") +
  geom_line(aes(y = ARMA34, color = "ARMA(3,4)")) +
  geom_line(aes(y = ARMA44, color = "ARMA(4,4)")) +
  geom_line(aes(y = AR5, color = "AR(5)")) +
  geom_line(aes(y = Naive, color = "Naive")) +
  labs(title = "Actual vs. Predicted Values",
       x = "Date",
       y = "Weekly Sales") +
  scale_color_manual(values = c("Actual" = "black",
                                "ARMA(3,4)" = "red",
                                "ARMA(4,4)" = "yellow",
                                "AR(5)" = "turquoise",
                                "Naive" = "blue"
                        
                                  
  )) +
  theme_minimal()

# Graphing the combined models

ggplot(forecast_data, aes(x = Date)) +
  geom_line(aes(y = Actual, color = "Actual"), linetype = "dashed") +
  geom_line(aes(y = Combined_Equal, color = "Combined Equal")) +
  geom_line(aes(y = Combined_Inverse, color = "Combined Inverse")) +
  geom_line(aes(y = Combined_OLS, color = "Combined OLS")) +
  labs(title = "Actual vs. Predicted Values",
       x = "Date",
       y = "Weekly Sales") +
  scale_color_manual(values = c(  "Combined Equal" = "pink",
                                  "Combined Inverse" = "blue",
                                  "Combined OLS" = "green"
                              
  )) +
  theme_minimal()

