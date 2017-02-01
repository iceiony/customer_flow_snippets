source('init.R')

shop_sales <- read.table('./data/prediction_example.csv', sep = ',',
                         col.names = c('shop_id', 1:14)) 

plot_sales(shop_sales, c(1,3))
