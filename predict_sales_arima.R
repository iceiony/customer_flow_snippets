source('init.R')

shop_sales <- read.table('./data/daily_shop_sales.csv', sep = ',', header = T)

library('forecast')

#shop_sales[is.na(shop_sales)] <- 0
sales <- unlist(shop_sales[1, -1])
start_day <- first(which(!is.na(sales)))
end_day   <- length(sales)
sales[is.na(sales)] <- 0
plot(sales,,'l')

opt_fit <- list()
opt_reg <- list()
idx <- 0
tic()
for(freq in c(365))
{
    series <- ts(sales[start_day:end_day], frequency = freq)

    cat(sprintf('\nfreq -> %d\n',freq));
    cat('\tord ->')
    for(i in seq(freq/3)){
        idx <- idx + 1
        cat(paste0(',',i));
        opt_reg[[idx]] <- fourier(series, K = i)
        opt_fit[[idx]] <- auto.arima(series, xreg = opt_reg[[idx]],
                                     #parallel  = TRUE,
                                     #num.cores = NULL,
                                     #stepwise  = FALSE,
                                     seasonal  = FALSE)
    }
}
toc()

errors <- sapply(opt_fit, f(x$aicc))
plot(errors,,'b')

for( idx in seq_along(opt_fit)){
    fit <- opt_fit[[idx]]
    reg <- opt_reg[[idx]]
    fc  <- forecast(fit, xreg = reg)
    dev.hold()
    plot(fc)
    week_window <- window(fc$mean, end = start(fc$mean) + 14/frequency(fc$mean))
    lines(week_window, col = 'red',lwd = 1.2)
    dev.flush()
    Sys.sleep(0.5)
}
