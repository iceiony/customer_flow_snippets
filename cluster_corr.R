source('./init.R')

mean_cor <- function(correlations){
    apply(correlations, 2, function(x){
        mean(x)
    })
}
cluster_cor <- function(cluster){
    if(nrow(cluster)==1) return(matrix(1))

#    cluster <- as.matrix2(cluster[,-(1:2)])
    cluster <- cluster[,-seq(ncol(cluster)-7*10)]
    cor(t(cluster), method = 'pearson', use = 'pairwise.complete.obs') 
}

shop_sales <- read.table('./data/daily_shop_sales.csv', sep = ',', header = T)
partition  <- read.table('./data/shop_clusters_180_city.csv', sep = ',', header = T)
shop_sales <- inner_join(partition, shop_sales, by = 'shop_id')

partition  <- c() 
all_cor    <- c()
for(cls in unique(shop_sales$cluster)){
    cluster <- filter(shop_sales, cluster == cls)
    cls_id  <- cluster$shop_id 

    partition <- bind_rows(
                    partition,
                    list(
                         shop_id = cls_id,
                         cor = mean_cor(cluster_cor(cluster))
                    ))
# chose random partitions of the same size 
    left_ids <- !shop_sales$shop_id %in% all_cor$shop_id 
    cls_id  <- sample(shop_sales$shop_id[left_ids], length(cls_id))
    cluster <- filter(shop_sales, shop_id %in% cls_id)

    all_cor <- bind_rows(
                    all_cor,
                    list(
                         shop_id = cls_id,
                         cor = mean_cor(cluster_cor(cluster))
                    ))
}

partition <- arrange(partition, shop_id)
all_cor <- arrange(all_cor, shop_id)

#plot_series(rbind(all = all_cor$cor, partition = partition$cor))
str_format <- '\tmean %0.2f\n\tvar %0.3f\n'
cat(paste0('Full cor\n',sprintf(str_format, mean(all_cor$cor), var(all_cor$cor))))
cat(paste0('Part cor\n',sprintf(str_format, mean(partition$cor), var(partition$cor))))
cat(paste0('Gain:', mean(partition$cor) - mean(all_cor$cor)))

