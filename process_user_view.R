source('init.R')

elapsed('Read data')

user_view_files <- c('./data/extra_user_view.txt', './data/user_view.txt')
view_data <- NULL
for(file_name in user_view_files){
    view_data <- rbind(view_data, 
                   read.table(file_name,
                              sep = ',',
                              col.names  = c('user_id', 'shop_id', 'time_stamp'),
                              colClasses = c('integer', 'integer', 'Date')) 
                 )
}

elapsed('Count date')
batches  <- 1:nrow(view_data) %/% 50000

view_data$day_nr <- unlist(
                        mclapply(split(view_data$time_stamp, batches), 
                                 function(x) as.numeric(x - start_date)))
view_data$time_stamp <- NULL
gc() #clear some memory

elapsed('Sort by day')
view_data <- view_data[order(view_data$day_nr),]

elapsed('Reshape shop view')
daily_shop_view <- group_by(view_data, shop_id, day_nr) %>%
                        summarise(count = n()) %>%
                        dcast(shop_id ~ day_nr, value.var = 'count')

elapsed('Output shop views')
write.table(daily_shop_view, './data/daily_shop_view.csv', sep = ',', na = '', row.names = F)

elapsed('Reshape user view')
daily_view <- group_by(view_data, user_id, day_nr) %>%
                 summarise(count = n())

elapsed('Saving user view count per day') 
write.table(daily_view, 'data/daily_view.csv', sep = ',' , na = '', row.names = F)

elapsed('Finished')
