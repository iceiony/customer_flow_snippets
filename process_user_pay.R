source('init.R')

elapsed('Read data')
all_sales <- read.table('./data/user_pay.txt', sep = ',', 
                        col.names  = c('user_id', 'shop_id', 'time_stamp'),
                        colClasses = c('integer', 'integer', 'Date'))

elapsed('Count date')
batches  <- 1:nrow(all_sales) %/% 50000

all_sales$day_nr <- unlist(
                        mclapply(split(all_sales$time_stamp, batches), 
                                 function(x) as.numeric(x - start_date)))
all_sales$time_stamp <- NULL
gc() #clear some memory

elapsed('Sort Records')
all_sales <- all_sales[order(all_sales$day_nr), ]

elapsed('Reshape shop sales data')
shop_sales <- group_by(all_sales, shop_id, day_nr) %>%
                 summarise(sales = n()) %>%
                 dcast(shop_id ~ day_nr, value.var = 'sales')

elapsed('Output shop sales')
write.table(shop_sales, './data/daily_shop_sales.csv', sep = ',', na = '', row.names = F)
rm(shop_sales)
gc() #clear some memory


elapsed('Reshape user buying data')
last_day <- max(all_sales$day_nr)
#split data by day and save to files for further processing
daily_sales <- sapply(1:last_day, 
               function(x){
                   file_name <- paste0('data/daily_sales/', x,'.csv')
                   print(paste0('Saving ', file_name))

                   day_sales <- filter(all_sales, day_nr == x)
                   write.table(day_sales, file_name, sep = ',', na = '', row.names = F)
                   file_name
               })
rm(all_sales)
gc()

user_buy <- bind_rows(
               lapply(1:last_day, 
                 function(x) {
                     print(paste0("Processing ", daily_sales[x]))
                     day_sales <- read.table(daily_sales[x],
                                             sep = ',',
                                             header = TRUE,
                                             colClasses = c('integer','integer','integer')
                                             )
                     group_by(day_sales, user_id) %>%
                         summarise(day_nr = x, buy = n())
                 })) 


elapsed('Output user purchase')
write.table(as.matrix(user_buy), './data/daily_user_buy.csv', sep = ',', na = '', row.names = F)

elapsed('Finished')
