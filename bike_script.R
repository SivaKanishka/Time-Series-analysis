library(Metrics)
library(tseries)
library(forecast)
library(dplyr)
library(ggplot2)

# Data has been taken from UCI machine learning database
# Data contains daily rental counts of a bike rental company along with several variables
daily_data = read.csv('day.csv', header=TRUE, stringsAsFactors=FALSE)

# Chaging the data type of Date variable
daily_data$Date = as.Date(daily_data$dteday)
# Plot of daily rental counts with time
ggplot(daily_data, aes(Date, cnt)) + geom_line() + scale_x_date('month')  + ylab("Daily Bike Checkouts") + xlab("")

# Changing the 'cnt' variable to time series object
count_ts = ts(daily_data[, c('cnt')])
# Removing outliers in data
daily_data$clean_cnt = tsclean(count_ts)
ggplot() + geom_line(data = daily_data, aes(x = Date, y = clean_cnt)) + ylab('Cleaned Bicycle Count')
# Smoothing of the data using moving averages (weekly and monthly)
daily_data$cnt_ma = ma(daily_data$clean_cnt, order=7) # using the clean count with no outliers
daily_data$cnt_ma30 = ma(daily_data$clean_cnt, order=30)
ggplot() +
  geom_line(data = daily_data, aes(x = Date, y = clean_cnt, colour = "Counts")) +
  geom_line(data = daily_data, aes(x = Date, y = cnt_ma,   colour = "Weekly Moving Average"))  +
  geom_line(data = daily_data, aes(x = Date, y = cnt_ma30, colour = "Monthly Moving Average"))  +
  ylab('Bicycle Count')

# Changing the weekly smoothed data to time series object 
count_ma = ts(na.omit(daily_data$cnt_ma), frequency=30)
# Decomposing
decomp = stl(count_ma, s.window="periodic")
# Removing the seasonality from the data
deseasonal_cnt <- seasadj(decomp)
plot(decomp)

# unit root test for stationarity with seasonal component
adf.test(count_ma, alternative = "stationary")

# Checking the ACF for checking stationarity of non-seasonal data
Acf(deseasonal_cnt, main='')
pacf(deseasonal_cnt, main ='')

# First order differencing
count_d1 = diff(deseasonal_cnt, differences = 1)
plot(count_d1)
plot(deseasonal_cnt)

# Unit root test of the first order differenced data
adf.test(count_d1, alternative = "stationary")

Acf(count_d1, main = 'ACF for Differenced Series')
pacf(count_d1, main = 'PACF for Differenced Series')

# fitting the data with ARIMA model
auto.arima(deseasonal_cnt, seasonal=FALSE)

fit <- auto.arima(deseasonal_cnt, seasonal=FALSE)

# Checking the residuals
tsdisplay(residuals(fit), lag.max=45, main='(1,1,1) Model Residuals')

# Changing the order of ARIMA
fit2 = arima(deseasonal_cnt, order=c(1,1,7))
tsdisplay(residuals(fit2), lag.max=15, main='Seasonal Model Residuals')
fit2

# Forecasting for 30 days
fcast <- forecast(fit2, h=30)
plot(fcast)

# Creating train and test sets for cross validation
hold <- window(ts(deseasonal_cnt), start=700)
fit_no_holdout = arima(ts(deseasonal_cnt[-c(700:725)]), order=c(1,1,7))
fcast_no_holdout <- forecast(fit_no_holdout,h=25)
plot(fcast_no_holdout, main=" ")
lines(ts(deseasonal_cnt))

# Fitting an ARIMA model for seasonal data
fit_w_seasonality = auto.arima(deseasonal_cnt, seasonal=TRUE)
fit_w_seasonality

# Forecasting
seas_fcast <- forecast(fit_w_seasonality, h=30)
plot(seas_fcast)

# Checking the residuals
tsdisplay(residuals(seas_fcast), lag.max=45, main='(1,1,1) Model Residuals')
