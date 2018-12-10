setwd("/Users/gorkemmeral/Documents")
sales <- read.csv("ukweborders.csv")
library('tidyverse')
library('forecast')
library('tseries')
sales$visit_date<-as.Date(sales$visit_date)
plot<-ggplot(sales, aes(visit_date, orders, group=1))+geom_line()+ ylab("Sales") + xlab("Date")
sales_ts = ts(sales[, c('orders')])
sales$clean_orders = tsclean(sales_ts)
clean_plot <- ggplot() + geom_line(data = sales, aes(x = visit_date, y = clean_orders, group=1)) + ylab('Cleaned Orders')

sales$ord_ma = ma(sales$clean_orders, order=7) # using the clean orders with no outliers
sales$ord_ma30 = ma(sales$clean_orders, order=30)
ggplot() +
  geom_line(data = sales, aes(x = visit_date, y = clean_orders, group=1, colour = "Orders")) +
  geom_line(data = sales, aes(x = visit_date, y = ord_ma, group=1,   colour = "Weekly Moving Average"))  +
  geom_line(data = sales, aes(x = visit_date, y = ord_ma30, group=1, colour = "Monthly Moving Average"))  +
  ylab('Orders') +xlab('Date')

#decomposition of the dataset
order_ma = ts(na.omit(sales$ord_ma), frequency=30)
decomp = stl(order_ma, s.window="periodic")
deseasonal_ord <- seasadj(decomp)
plot(decomp)

#ADF test
adf.test(order_ma, alternative = "stationary")

#Autocorrelations
Acf(order_ma, main='')

Pacf(order_ma, main='')

#Model fit
fit <- auto.arima(deseasonal_ord, seasonal=FALSE)

tsdisplay(residuals(fit), lag.max=45, main='(2,1,2) Model Residuals')

#ARIMA (1,2,8) is a better fit than auto.arima
fit2 = arima(deseasonal_ord, order=c(1,2,8))
tsdisplay(residuals(fit2), lag.max=45, main='(1,2,8) Model Residuals')

#Forecast
fcast <- forecast(fit2, h=30)
plot(fcast)


#Compare the forecast to the actual observed values
hold <- window(ts(deseasonal_ord), start=400)

fit_no_holdout = arima(ts(deseasonal_ord[-c(400:415)]), order=c(1,2,8))

fcast_no_holdout <- forecast(fit_no_holdout,h=25)
plot(fcast_no_holdout, main=" ")
lines(ts(deseasonal_ord))

#Fit with seasonality

fit_w_seasonality <- auto.arima(deseasonal_ord, seasonal=TRUE)
fit_w_seasonality

#Forecast with seasonality
seas_fcast <- forecast(fit_w_seasonality, h=30)
<<<<<<< HEAD
plot(seas_fcast)
=======
plot(seas_fcast)
>>>>>>> 01560368553634a4affe5f3823048b10b37ed532
