source('init.R')

shop_sales <- read.table('./data/prediction_example.csv', sep = ',',
                         col.names = c('shop_id', 1:14)) 

plot_sales(shop_sales, c(1,3))

shop_views <- read.table('./data/daily_shop_view.csv', sep = ',', header = T)
shop_sales <- read.table('./data/daily_shop_sales.csv', sep = ',', header = T)

dat <- as.matrix(shop_views[,-1])
dev.new()
heatmap(dat, main = 'shop views', 
        Rowv = NA, Colv = NA,
        ylab = 'shops', xlab = 'days',
        labRow = NA, labCol = NA)

dat <- as.matrix(shop_sales[,-1])
dev.new()
heatmap(dat, main = 'shop sales',  
        Rowv = NA, Colv = NA,
        ylab = 'shops', xlab = 'days',
        labRow = NA, labCol = NA)

