source('init.R')

elapsed('Data Read')
all_sales <- read.table('./data/user_pay.txt', sep = ',', 
                        col.names  = c('user_id', 'shop_id', 'time_stamp'),
                        colClasses = c('integer', 'integer', 'Date'))

elapsed('Days counted')
batches  <- 1:nrow(all_sales) %/% 10000
start_date <- as.Date('2015-07-01')
all_sales$day_nr <- unlist(
                        mclapply(split(all_sales$time_stamp, batches), 
                                 function(x) as.numeric(x - start_date)))

elapsed('Records sorted')
all_sales <- all_sales[order(all_sales$day_nr), ]

elapsed('Reshape shop sales data')
shop_sales <- group_by(all_sales, shop_id, day_nr) %>%
                 summarise(sales = n()) %>%
                 dcast(shop_id ~ day_nr, value.var = 'sales')
#add missing date as NA
shop_sales$"164" <- NA

elapsed('Output shop sales')
write.table(shop_sales, './data/shop_sales.csv', sep = ',', na = '', row.names = F)

elapsed('Reshape user buying data')
user_buy <- group_by(all_sales, user_id, day_nr) %>% 
                summarise(buy = n()) %>%
                dcast(user_id ~ day_nr, value.var = 'buy', fill = 0)
#add missing dae as NA
user_buy$"164" <- 0

elapsed('Output user purchase')
write.table(user_buy, './data/user_buy.csv', sep = ',', na = '', row.names = F)

elapsed('Finished')
