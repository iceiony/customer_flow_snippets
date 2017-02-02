source('init.R')

elapsed('Read Data')
all_sales <- read.table('./data/user_pay.txt', sep = ',', 
                        col.names  = c('user_id', 'shop_id', 'time_stamp'),
                        colClasses = c('integer', 'integer', 'Date'))

elapsed('Count date')
batches  <- 1:nrow(all_sales) %/% 50000
start_date <- as.Date('2015-06-25')
all_sales$day_nr <- unlist(
                        mclapply(split(all_sales$time_stamp, batches), 
                                 function(x) as.numeric(x - start_date)))

elapsed('Sort Records')
all_sales <- all_sales[order(all_sales$day_nr), ]

elapsed('Reshape shop sales data')
shop_sales <- group_by(all_sales, shop_id, day_nr) %>%
                 summarise(sales = n()) %>%
                 dcast(shop_id ~ day_nr, value.var = 'sales')

elapsed('Output shop sales')
write.table(shop_sales, './data/shop_sales.csv', sep = ',', na = '', row.names = F)
rm(shop_sales)

elapsed('Reshape user buying data')
all_sales$time_stamp <- NULL
last_day <- max(all_sales$day_nr)
user_buy <- bind_rows(
               mclapply(as.list(1:last_day), 
                 function(x) {
                     filter(all_sales, day_nr == x) %>%
                         group_by(user_id) %>%
                         summarise(buy = n(), day_nr = x)
                 })) 


elapsed('Output user purchase')
write.table(as.matrix(user_buy), './data/user_buy.csv', sep = ',', na = '', row.names = F)

elapsed('Finished')
