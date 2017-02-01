source('init.R')

shop_sales <- read.csv('./data/prediction_example.csv', sep = ',', header = FALSE) 
colnames(shop_sales) <- c('shop_id', 1:14)

plot_sales(shop_sales, c(1,3))


