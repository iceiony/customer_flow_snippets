source('init.R')

source('./cluster_shops.R')

shop_sales <- read.table('./data/daily_shop_sales.csv', sep = ',', header = T)
