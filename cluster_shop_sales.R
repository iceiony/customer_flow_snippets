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

save(fit, file = 'pvclust_corr.Rdata')

classes <- pvpick(fit, alpha = 0.85)
lapply(classes$clusters, write, './data/pvclust_85.csv', append=T, ncolumns = 2000, sep=',')
