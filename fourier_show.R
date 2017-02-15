source('./init.R')

shop_sales  <- read.table('./data/daily_shop_sales.csv', sep = ',', header = T)
shop_groups <- read.table('./data/shop_clusters.csv', sep = ',', header = T)

sales <- as.matrix2(shop_sales)
sales[is.na(sales)] <- 0


freq <- 365
L  <- length(sales[1,-1]) 
hz <- c(0 , seq(L/2))
hz <- hz * freq / L
shop1 <- abs(fft(sales[1,-1])) / L
plot(hz, shop1[seq_along(hz)], 'l')
#clean <- fft(shop1, inverse = T)
plot_series(sales, 1)
