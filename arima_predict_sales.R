source('init.R')
library('forecast')

shop_sales <- read.table('./data/daily_shop_sales.csv', sep = ',', header = T)

frequencies <- c(365, 182, 91, 30, 14)
freq_orders <- c(120, 90, 60, 14, 7)

#shop_sales[is.na(shop_sales)] <- 0
#
forecasts <- mclapply(seq(nrow(shop_sales)), mc.cores = 8, mc.preschedule = F,
               function(idx){
                    cat(paste0(' ', idx,'\n'));

                    tic()
                    sales <- unlist(shop_sales[idx, -1])
                    start_day <- first(which(!is.na(sales)))
                    end_day   <- length(sales)
                    duration <- end_day - start_day
                    sales[is.na(sales)] <- 0

                    braket <- first(which(duration > frequencies))
                    freq <- frequencies[braket]
                    ord  <- freq_orders[braket]

                    series  <- ts(sales[start_day:end_day], frequency = freq)
                    regress <- fourier(series, K = ord)
                    fit     <- auto.arima(series, xreg = regress, seasonal = FALSE)
                    fc      <- forecast(fit, xreg = regress)

                    png(sprintf('./estimates/%d.png', idx), width = 2000, height = 1000)
                    plot(fc)
                    week <- window(fc$mean, end = start(fc$mean) + 13/frequency(fc$mean))
                    lines(week, col = 'red', lwd = 1.2)
                    dev.off()
                    toc()

                    result <- c(shop_sales$shop_id[idx], fc$mean[1:14]) 
                    write.table(result,
                                sprintf('./estimates/%d.csv',idx),
                                sep = ',', col.names = F, row.names = F)

                    return(result)
               }
             )

forecasts <- do.call(rbind,forecasts)
colnames(forecasts) <- c('shop_id', sapply(1:14, f(paste0('day_',x))))

write.table(forecasts,'./estimates/predictions.csv', sep=',', 
            col.names = F, row.names = F)
