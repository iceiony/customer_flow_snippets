source('init.R')
library('pvclust')

header_info = c('shop_id', 'city_name', 'location_id',
                'per_pay', 'score', 'comment_cnt', 'shop_level',
                'cate_1_name', 'cate_2_name', 'cate_3_name')
shop_info <- read.table('./data/shop_info.txt', sep = ',', 
                        header = F, col.names = header_info)

shop_info$shop_level <- as.factor(shop_info$shop_level)
shop_info$location_id <- as.factor(shop_info$location_id)

dat <- select(shop_info, per_pay, score) %>% scale()

columns <- c('shop_level', 'city_name', 'location_id', 
             'cate_1_name', 'cate_2_name', 'cate_3_name')
for(name in columns){
    new_col <- sapply(shop_info[[name]], 
                 f(x, x == levels(shop_info[[name]]))
               ) %>% t()
    colnames(new_col) <- sapply(1:ncol(new_col), f(x, paste0(name,'_',x)))
    dat <- cbind(dat, new_col)
}

dat <- data.matrix(dat) %>% t()
dat[is.na(dat)] <- 2.5

colnames(dat) <- shop_info$shop_id

fit <- pvclust(dat, method.hclust = 'ward.D2', parallel = T, method.dist = 'euclidean') 

save(fit, file = 'info_pvclust_corr.Rdata')

classes  <- pvpick(fit, alpha = 0.85)
clusters <- lapply(seq_along(classes$clusters), 
                  function(idx){
                      shop_id = as.numeric(classes$clusters[[idx]])
                      data.frame(shop_id, cluster = idx)
                  }) %>% bind_rows() %>% arrange(shop_id)

nrow(clusters)

write.table(clusters, 'data/info_pvclust_85.csv', sep = ',', row.names = F)
