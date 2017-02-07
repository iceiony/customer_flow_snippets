source('init.R')

header_info = c('shop_id', 'city_name', 'location_id',
                'per_pay', 'score', 'comment_cnt', 'shop_level',
                'cate_1_name', 'cate_2_name', 'cate_3_name')
shop_info <- read.table('./data/shop_info.txt', sep = ',', 
                        header = F, col.names = header_info)

shop_info[is.na(shop_info)] <- 0
dat <- (data.matrix(shop_info))

wss <- rep(Inf,20) 
for(i in seq_along(wss)){
    for( trials in seq(5)){
        clust <- kmeans(dat, centers = i, iter.max = 200) 
        wss[i] <- min(sum(clust$withinss), wss[i])
    }
}

dev.new()
plot(seq_along(wss), wss, type = 'b',
     xlab = 'Nr Clusters',
     ylab = 'Distance within groups')


