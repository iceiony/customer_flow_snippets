source('init.R')

header_info = c('shop_id', 'city_name', 'location_id',
                'per_pay', 'score', 'comment_cnt', 'shop_level',
                'cate_1_name', 'cate_2_name', 'cate_3_name')
shop_info <- read.table('./data/shop_info.txt', sep = ',', 
                        header = F, col.names = header_info)

shop_info[is.na(shop_info)] <- 0
dat <- scale(data.matrix(shop_info[,-1]))

wss <- rep(Inf,20) 
for(i in seq_along(wss)){
    for( trials in seq(5)){
        clust <- kmeans(dat, centers = i, iter.max = 200) 
        wss[i] <- min(sum(clust$withinss), wss[i])
    }
}


png('figures/cluster_distances.png')
plot(seq_along(wss), wss, type = 'b',
     xlab = 'Nr Clusters',
     ylab = 'Distance within groups')
dev.off()


optim_cnt <- 3
optim_cls <- NULL
min_dist    <- Inf
for(trials in seq(5)){
    clust <- kmeans(dat,centers = optim_cnt, iter.max = 200)
    dist  <- sum(clust$withinss)
    if(min_dist > dist){
        min_dist <- dist 
        optim_cls <- clust
    }
}

message('Optimal ', optim_cnt, ' cluster size with error ', min_dist)
shop_clusters <- data.frame(shop_id = shop_info$shop_id, cluster = optim_cls$cluster)

library('cluster')
png('figures/cluster_by_pca_3_class.png', width = 700, height = 700)
clusplot(dat, shop_clusters$cluster, color = T, shade = T, lines = 0)
dev.off()

library('fpc')
png('figures/cluster_by_discrim_3_class.png', width = 700, height = 700)
plotcluster(dat, shop_clusters$cluster, clnum = 3, method = 'dc')
dev.off()

write.table(shop_clusters, 'data/shop_clusters.csv', sep = ',' , row.names = F)
