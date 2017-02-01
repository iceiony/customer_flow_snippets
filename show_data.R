source('init.R')

#shop_sales <- read.table('./data/prediction_example.csv', sep = ',',
#                         col.names = c('shop_id', 1:14)) 
#
#plot_sales(shop_sales, c(1,3))
tic();
all_sales <- read.table('./data/sample_pay.txt', sep = ',', 
                        col.names  = c('user_id', 'shop_id', 'time_stamp'),
                        colClasses = c('integer', 'integer', 'Date'))
message('Data Read')
elapsed()

batches  <- 1:nrow(all_sales) %/% 10000
start_date <- as.Date('2015-07-01')
all_sales$nr_days <- unlist(
                        mclapply(split(all_sales$time_stamp, batches), 
                                 function(x) as.numeric(x - start_date)))
message('Days counted')
elapsed()

all_sales <- all_sales[order(all_sales$nr_days), ]
message('Records sorted')
elapsed()


