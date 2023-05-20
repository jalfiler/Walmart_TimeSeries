
#Step 2. Perform out-of-sample evaluations
#You will describe your forecasting environment and make the following options: 
#one-step ahead forecast (h=1), split your into two parts. 
#Using the first 90% sample as estimation sample and the rest as prediction sample.
#use the fixed sampling scheme, consider at least three models 
#(at least one of them is ARMA models, which could be AR,MA, or ARMA). 
#You will use quadratic loss function and Mean Squared Error (MSE) to choose the optimal forecast. 
#You will implement the forecast optimality tests (MPE and informational efficiency tests) 
#for each model, discard any model if necessary, 
#and add a simpler forecast that is calculated by averaging the last four observations, 
#ft,1 = (yt + yt−1 + yt−2 + yt−3) /4.
#You will implement the test of unconditional predictability and explain which forecast is preferred. 
#You will make a combined forecast from the ARMA models and the simpler moving average forecast using the OLS weighted combination scheme. 
#You will show the MSE of three forecasts and comment on which one you prefer.

library(tidyverse) 
library(lubridate) 
library(stringr) 
library(forecast) 
library(dplyr)
library(ggplot2)
library(tseries)
library(dynlm) 


df <- read.csv('walmart_cleaned.csv')

# Convert the "Date" column to a Date format
# Extract day, month, and year components from the Date column
# group by store and date

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
#print(length(time_series))

# Perform regular differencing - first-order difference
diff_time_series <- diff(time_series, differences = 1)
#print(length(diff_time_series))

# Perform seasonal differencing - DO WE KEEP THIS TRANSFORMATION?
#diff_time_series <- diff(diff_time_series, differences = 1, lag = 52) 
# seasonal differencing based on the yearly frequency.

#Determine the index corresponding to the 90% cutoff point. 
#Since data is weekly, the index will represent the number of observations, not specific dates. 
cutoff_index <- round(0.9 * length(diff_time_series))

#Fitting the models 
#AR(3) model
AR3_model <- arima(diff_time_series[1:cutoff_index], order=c(3,0,0)) 
summary(AR3_model)

#ARMA(1,1) model
ARMA11_model <- arima(diff_time_series[1:cutoff_index], order=c(1,0,1)) 
summary(ARMA11_model)

#Moving Average model
MA_model <- arima(diff_time_series[1:cutoff_index], order=c(0,0,1)) 
summary(MA_model)

#ARMA(1,3) model
ARMA13_model <- arima(diff_time_series[1:cutoff_index], order=c(1,0,3)) 
summary(ARMA13_model)


#Initialize empty vectors to store forecasted values, forecast errors, and squared losses
n_prediction <- length(diff_time_series) - cutoff_index
fcast1 <- fcast2 <- fcast3 <- fcast4 <- fcast5 <- numeric(n_prediction)
ferror1 <- ferror2 <- ferror3 <- ferror4 <- ferror5 <- numeric(n_prediction)
loss1 <- loss2 <- loss3 <- loss4 <- loss5 <-numeric(n_prediction)

# one-step ahead forecast (h=1) means that you are predicting the next value in the time series based on the current available data
#A loop to generate forecasted values, forecast errors, and squared losses for all four models:
for (i in 0:(n_prediction-1)) {
  
  # Update the models with the newly observed data point
  AR3_model <- arima(diff_time_series[1:(cutoff_index+i)], order=c(3,0,0)) 
  ARMA11_model <- arima(diff_time_series[1:(cutoff_index+i)], order=c(1,0,1))
  MA_model <- arima(diff_time_series[1:(cutoff_index+i)], order=c(0,0,1)) 
  ARMA13_model <- arima(diff_time_series[1:(cutoff_index+i)], order=c(1,0,3))
  
  
  # AR(3) model
  fcast1[i+1] <- predict(AR3_model, n.ahead = 1)$pred
  ferror1[i+1] <- diff_time_series[i + cutoff_index + 1] - fcast1[i+1]
  loss1[i+1] <- ferror1[i+1]^2
  
  # ARMA(1,1) model
  fcast2[i+1] <- predict(ARMA11_model, n.ahead = 1)$pred
  ferror2[i+1] <- diff_time_series[i + cutoff_index + 1] - fcast2[i+1]
  loss2[i+1] <- ferror2[i+1]^2
  
  # Moving Average model (MA1)
  fcast3[i+1] <- predict(MA_model, n.ahead = 1)$pred
  ferror3[i+1] <- diff_time_series[i + cutoff_index + 1] - fcast3[i+1]
  loss3[i+1] <- ferror3[i+1]^2
  
  # ARMA(1,3) model
  fcast4[i+1] <- predict(ARMA13_model, n.ahead = 1)$pred
  ferror4[i+1] <- diff_time_series[i + cutoff_index + 1] - fcast4[i+1]
  loss4[i+1] <- ferror4[i+1]^2
  
  
  # Simple average of last 4 observations
  fcast5[i+1] <- mean(diff_time_series[(i+cutoff_index-3):(i+cutoff_index)])
  ferror5[i+1] <- diff_time_series[i + cutoff_index + 1] - fcast5[i+1]
  loss5[i+1] <- ferror5[i+1]^2
  
  
}

#NOT SURE WE"LL NEED THIS, but she did similar thing in her original code
#Combining the forecasted values, forecast errors, and squared losses into a single dataframe:
results1 <- data.frame(fcast = fcast1, ferror = ferror1, loss = loss1)
results2 <- data.frame(fcast = fcast2, ferror = ferror2, loss = loss2)
results3 <- data.frame(fcast = fcast3, ferror = ferror3, loss = loss3)
results4 <- data.frame(fcast = fcast4, ferror = ferror4, loss = loss4)
results5 <- data.frame(fcast = fcast5, ferror = ferror5, loss = loss5)


# Combine all results into a single data frame:
all_results <- rbind(cbind(model = "AR(3)", results1),
                     cbind(model = "ARMA(1,1)", results2),
                     cbind(model = "MA", results3),
                     cbind(model = "ARMA(1,3)", results4),
                     cbind(model = "Last 4 Obs. Avg.", results5)
                     )


# Calculate MSE for each model
#average squared difference between the estimated values and the actual value. 
#A model with a smaller MSE is generally considered better.
MSE1 <- mean(loss1)
MSE2 <- mean(loss2)
MSE3 <- mean(loss3)
MSE4 <- mean(loss4)
MSE5 <- mean(loss5)



# Create a dataframe with MSE values
mse_df <- data.frame(Model = c("AR(3)", "ARMA(1,1)", "MA", "ARMA(1,3)", "Last 4 Obs. Avg." ),
                     MSE = c(MSE1, MSE2, MSE3, MSE4, MSE5))

# Print the combined dataframe
mse_df


# Calculate MAE for each model to test unconditional predictability 
# average absolute difference between the forecasted values and the actual values
# We then compare the MAE values and determine if the simpler moving average forecast (model 5) 
# has the lowest MAE. If it does, we can conclude that the simpler forecast demonstrates unconditional predictability and is preferred over the other models.
MAE1 <- mean(abs(ferror1))
MAE2 <- mean(abs(ferror2))
MAE3 <- mean(abs(ferror3))
MAE4 <- mean(abs(ferror4))
MAE5 <- mean(abs(ferror5))


# Create a dataframe with MSE values
mae_df <- data.frame(Model = c("AR(3)", "ARMA(1,1)", "MA", "ARMA(1,3)", "Last 4 Obs. Avg." ),
                     MSE = c(MAE1, MAE2, MAE3, MAE4, MAE5))

# Print the combined dataframe
mae_df




# Perform MPE and IE tests for each model
# MPE (Mean Percentage Error) represents the average percentage error in the forecasts. 
#A perfect forecast would result in an MPE of 0, 
#while forecasts that are too low will result in a positive MPE, 
#and forecasts that are too high will result in a negative MPE.

#Informational Efficiency Test (IETest): 
#checks if the forecast errors are correlated with past forecasted values. 
#If the forecast errors are uncorrelated with past forecasted values, 
#it suggests that all available information has been used and the forecasts are informationally efficient. 
#This is done by regressing the forecast errors on the forecasted values. 
#If the coefficient of the forecasted values is statistically significant, 
#it suggests that the forecasts are not informationally efficient.

mpetest1 <- lm(ferror1 ~ 1)
summary(mpetest1)

IETest1 <- lm(ferror1 ~ fcast1)
summary(IETest1)

mpetest2 <- lm(ferror2 ~ 1)
summary(mpetest2)

IETest2 <- lm(ferror2 ~ fcast2)
summary(IETest2)

mpetest3 <- lm(ferror3 ~ 1)
summary(mpetest3)

IETest3 <- lm(ferror3 ~ fcast3)
summary(IETest3)

mpetest4 <- lm(ferror4 ~ 1)
summary(mpetest4)

IETest4 <- lm(ferror4 ~ fcast4)
summary(IETest4)


mpetest5 <- lm(ferror5 ~ 1)
summary(mpetest5)

IETest5 <- lm(ferror5 ~ fcast5)
summary(IETest5)



#When selecting the best model, you would generally prefer the one with the smallest MSE, 
#as this indicates that the model's predictions are, on average, very close to the actual values. 
#Additionally, you would prefer a model where the IETest shows that the forecasts are informationally efficient 
#(i.e., the coefficient of the forecasted values is not statistically significant). 
#The MPE can also be useful in determining the best model, 
#but it might be less reliable than the MSE because it can be skewed by a few large percentage errors.



# You will make a combined forecast from the ARMA models and the simpler moving average 
# forecast using the OLS weighted combination scheme.


# Finding the weights for OLS weighted combination scheme

# Initialize empty vectors 
fcast_combined <- numeric(n_prediction)
ferror_combined <- numeric(n_prediction)
loss_combined <- numeric(n_prediction)

# Matrix X that contains the forecasted values from each individual model
#X <- cbind(fcast1, fcast2, fcast3, fcast4, fcast5)

# Actual values for the forecasted values.
#y <- diff_time_series[(cutoff_index + 1):(cutoff_index + n_prediction)]
#model <- lm(y ~ X)
#weights <- coef(model)[-1]  # Exclude intercept term
#weights <- weights / sum(weights)  # Normalize the weights

# fixed weights for each model
weights <- c(0.2, 0.2, 0.2, 0.2, 0.2)  


for (i in 1:n_prediction) {
  
  # Combined forecast using OLS weighted combination
  fcast_combined[i] <- sum(weights * X[i, ])
  ferror_combined[i] <- y[i] - fcast_combined[i]
  loss_combined[i] <- ferror_combined[i]^2
}

result_combined <- data.frame(fcast = fcast_combined, ferror = ferror_combined, loss = loss_combined)
MSE_combined <- mean(loss_combined)


# Create a data frame with predicted values and actual values

df_diff <- data.frame(Date = sales$Date[-1], Value = diff_time_series)

forecast_data <- data.frame(
  Date = df_diff$Date[(cutoff_index + 1):(cutoff_index + n_prediction)],
  Actual = df_diff$Value[(cutoff_index + 1):(cutoff_index + n_prediction)],
  AR3 = fcast1,
  ARMA11 = fcast2,
  MA = fcast3,
  ARMA13 = fcast4,
  Last4Avg = fcast5,
  Combined = fcast_combined
)

# Create a plot of the predicted values and actual values
# Create the time series plot

ggplot(forecast_data, aes(x = Date)) +
  geom_line(aes(y = Actual, color = "Actual"), linetype = "dashed") +
  geom_line(aes(y = AR3, color = "AR(3)")) +
  geom_line(aes(y = ARMA11, color = "ARMA(1,1)")) +
  geom_line(aes(y = MA, color = "MA")) +
  geom_line(aes(y = ARMA13, color = "ARMA(1,3)")) +
  geom_line(aes(y = Last4Avg, color = "Last 4 Obs. Avg.")) +
  geom_line(aes(y = Combined, color = "Combined")) +
  labs(title = "Actual vs. Predicted Values",
       x = "Date",
       y = "Weekly Sales") +
  scale_color_manual(values = c("Actual" = "black",
                                "AR(3)" = "blue",
                                "ARMA(1,1)" = "red",
                                "MA" = "green",
                                "ARMA(1,3)" = "purple",
                                "Last 4 Obs. Avg." = "orange",
                                "Combined" = "magenta")) +
  theme_minimal()












