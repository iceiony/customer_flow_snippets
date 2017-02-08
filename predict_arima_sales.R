source('init.R')

shop_sales <- read.table('./data/daily_shop_sales.csv', sep = ',', header = T)

library('forecast')

#shop_sales[is.na(shop_sales)] <- 0

sales <- unlist(shop_sales[5,-1])
freq  <- 365.25/6
first_record <- 1/freq 
sales <- ts(sales, freq = freq, start = first_record)
plot(sales)

freq_orders <- 7
fit <- auto.arima(sales, xreg=fourier(sales, K = freq_orders), seasonal = F)
fc  <- forecast(fit, xreg = fourier(sales,freq_orders))

plot(fc)
week_window <- window(fc$mean, end = start(fc$mean) + 14/freq)
lines(week_window, col = 'red',lwd = 1.2)
