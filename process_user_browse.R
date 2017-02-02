source('init.R')

elapsed('Read data')

user_view_files <- c('./data/extra_user_view.txt', './data/user_view.txt')
user_view <- NULL
for(f in user_view_files){
    user_view <- rbind(user_view, 
                   read.table('./data/extra_user_view.txt', sep = ',',
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

elapsed('Show histogram')
hist(unlist(user_view$day_nr),xlim = c(0,500), main = 'all user_view') 
