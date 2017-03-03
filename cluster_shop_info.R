source('init.R')
library('pvclust')

header_info = c('shop_id', 'city_name', 'location_id',
                'per_pay', 'score', 'comment_cnt', 'shop_level',
                'cate_1_name', 'cate_2_name', 'cate_3_name')
shop_info <- read.table('./data/shop_info.txt', sep = ',', 
                        header = F, col.names = header_info)

shop_info[is.na(shop_info)] <- 0
shop_info$shop_level <- as.factor(shop_info$shop_level)
shop_info$location_id <- as.factor(shop_info$location_id)

dat    <- select(shop_info, per_pay, score) 

columns <- c('shop_level', 'cate_1_name', 'cate_2_name', 'cate_3_name')
for(name in columns){
    new_col <- sapply(shop_info[[name]], 
                 f(x, x == levels(shop_info[[name]]))
               ) %>% t()
    colnames(new_col) <- sapply(1:ncol(new_col), f(x, paste0(name,'_',x)))
    dat <- cbind(dat, new_col)
}

dat <- scale(data.matrix(dat))

rownames(dat) <- 1:nrow(dat)

wss <- rep(Inf,250) 
for(i in seq_along(wss)){
    for( trials in seq(5)){
        #fit <- pvclust(t(dat), method.hclust = 'ward.D2', method.dist = 'euclidean', parallel = T) 
        clust <- kmeans(dat, centers = i, iter.max = 200) 
        wss[i] <- min(sum(clust$withinss), wss[i])
    }
}


png('figures/cluster_distances.png')
plot(seq_along(wss), wss, type = 'b',
     xlab = 'Nr Clusters',
     ylab = 'Distance within groups')
dev.off()


optim_cnt <- 90#70#84
optim_cls <- NULL
min_dist    <- Inf
for(trials in seq(10)){
    clust <- kmeans(dat,centers = optim_cnt, iter.max = 200)
    dist  <- sum(clust$withinss)
    if(min_dist > dist){
        min_dist <- dist 
        optim_cls <- clust
    }
}
message('Optimal ', optim_cnt, ' cluster size with error ', min_dist)
hist(optim_cls$cluster, breaks=optim_cnt)

#library('cluster')
#png('figures/cluster_by_pca_7_class.png', width = 700, height = 700)
#clusplot(dat, optim_cls$cluster, color = T, shade = T, lines = 0)
#dev.off()
#
#library('fpc')
#png('figures/cluster_by_discrim_7_class.png', width = 700, height = 700)
#plotcluster(dat, optim_cls$cluster, clnum = 1, method = 'dc')
#dev.off()

shop_clusters <- data.frame(shop_id = shop_info$shop_id,
                            cluster = optim_cls$cluster)   
write.table(shop_clusters, 'data/shop_clusters_90.csv', sep = ',' , row.names = F)
