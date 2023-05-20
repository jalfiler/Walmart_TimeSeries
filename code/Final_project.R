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
  labs(x = "Date", y = "Total weekly sales", title = "Weekly sales time series for Store 1")


# Perform the ADF test - reject the null hypothesis of non-stationarity and conclude that the time series is stationary.
adf.test(time_series)

# Perform the KPSS test -  reject the null hypothesis of level stationarity and conclude that the time series is non-stationary.
kpss.test(time_series)

# Perform seasonal differencing
diff_time_series <- diff(time_series,differences = 1, lag = 12)

# Plot the differenced time series
plot(diff_time_series, main = "Differenced time series")

# Perform the ADF test after seasonal differencing
adf.test(diff_time_series)

# Perform the KPSS test after seasonal differencing
kpss.test(diff_time_series)


# Create an autocorrelation plot
acf(diff_time_series)

# Create a partial autocorrelation plot
pacf(diff_time_series)


# Fit MA(4) model
model_ma4 <- arima(diff_time_series, order=c(0,0,4))
summary_ma4 <- summary(model_ma4)
print(summary_ma4)
# Fit AR(3) model
model_ar3 <- arima(diff_time_series, order=c(3,0,0))
summary_ar3 <- summary(model_ar3)
print(summary_ar3)
# Fit AR(4) model
model_ar4 <- arima(diff_time_series, order=c(4,0,0))
summary_ar4 <- summary(model_ar4)
print(summary_ar4)
# Fit AR(5) model
model_ar5 <- arima(diff_time_series, order=c(5,0,0))
summary_ar5 <- summary(model_ar5)
print(summary_ar5)
# Fit ARMA(2,2) model
model_arma22 <- arima(diff_time_series, order=c(2,0,2))
summary_arma22 <- summary(model_arma22)
print(summary_arma22)
# Fit ARMA(2,4) model
model_arma24 <- arima(diff_time_series, order=c(2,0,4))
summary_arma24 <- summary(model_arma24)
print(summary_arma24)
model_summary <- data.frame(
  Model = c("MA(4)", "AR(3)", "AR(4)", "AR(5)", "ARMA(2,2)", "ARMA(2,4)"),
  AIC = c(summary_ma4$aic, summary_ar3$aic, summary_ar4$aic, summary_ar5$aic, summary_arma22$aic, summary_arma24$aic),
  BIC = c(AIC(model_ma4, k = log(length(diff_time_series))),
          AIC(model_ar3, k = log(length(diff_time_series))),
          AIC(model_ar4, k = log(length(diff_time_series))),
          AIC(model_ar5, k = log(length(diff_time_series))),
          AIC(model_arma22, k = log(length(diff_time_series))),
          AIC(model_arma24, k = log(length(diff_time_series))))
)
model_summary