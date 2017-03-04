source('init.R')
library('pvclust')

shop_sales  <- read.table('./data/daily_shop_sales.csv', sep = ',', header = T)

dat <- scale(data.matrix(shop_sales[,-1]) %>% t())
colnames(dat) <- shop_sales[,1]
fit <- pvclust(dat, method.hclust = 'ward.D2', parallel = T) 

#png('figures/hierarhical_clusters.png', width = 20000, height = 2000)
#plot(fit)
#pvrect(fit)
#dev.off()

save(fit, file = 'sales_pvclust_corr.Rdata')

classes  <- pvpick(fit, alpha = 0.95)
clusters <- lapply(seq_along(classes$clusters), 
                  function(idx){
                      shop_id = as.numeric(classes$clusters[[idx]])
                      data.frame(shop_id, cluster = idx)
                  }) %>% bind_rows() %>% arrange(shop_id)

nrow(clusters)

write.table(clusters, 'data/sales_pvclust_95.csv', sep=',', row.names = F)
