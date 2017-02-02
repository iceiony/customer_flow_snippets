source('init.R')

elapsed('Read data')

user_view_files <- c('./data/extra_user_view.txt', './data/user_view.txt')
user_view <- NULL
for(file_name in user_view_files){
    user_view <- rbind(user_view, 
                   read.table(file_name,
                              sep = ',',
                              col.names  = c('user_id', 'shop_id', 'time_stamp'),
                              colClasses = c('integer', 'integer', 'Date')) 
                 )
}

elapsed('Count date')
batches  <- 1:nrow(user_view) %/% 50000

user_view$day_nr <- unlist(
                        mclapply(split(user_view$time_stamp, batches), 
                                 function(x) as.numeric(x - start_date)))
user_view$time_stamp <- NULL
gc() #clear some memory

elapsed('Sort by day')
user_view <-  user_view[order(user_view$day_nr),]

elapsed('Reshape user view')
daily_view <- group_by(user_view, user_id, day_nr) %>%
                 summarise(count = n())

elapsed('Saving user view count per day') 
write.table(daily_view, 'data/daily_view.csv', sep = ',' , na = '', row.names = F)

elapsed('Finished')
