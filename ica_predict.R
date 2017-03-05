source('init.R')
source('./mlp_functions/init.R')
library('smoother')
library('fastICA')
library('forecast')
library('MASS')

shop_sales <- read.table('./data/daily_shop_sales.csv', sep = ',', header = T)
shop_cls   <- read.table('./data/sales_pvclust_95.csv', sep = ',', header = T)

shop_sales <- join(shop_sales, shop_cls, by = 'shop_id')
shop_sales <- shop_sales[!is.na(shop_sales$cluster),]

prepare <- function(shop_sales, cls, trim = 0, ws = 0.04){
                group  <- (filter(shop_sales, cluster == cls)) %>% 
                            apply(1,function(x) {
                               x <- x[-seq(2)] %>% smth.gaussian(ws, tails=T)
                               if(trim==0) return(x)
                               head(x, -trim)
                            }) %>% t() 
                group <- group[,-seq(250)]
                group[is.na(group)] <- 0
                group
            }


scores  <-  mclapply(sort(unique(shop_sales$cluster)), mc.cores = 7,
            function(cls){
                message(cls)
                train <- prepare(shop_sales, cls, 14)
                valid <- prepare(shop_sales, cls, 0)

                a <- fastICA(t(train), nrow(train))
                S <- apply(a$S, 2, function(s){
                                fit <- auto.arima(s, seasonal = T) 
                                fc  <- forecast(fit, 14)
                                c(s , fc$mean)
                          })

                M <- apply(train, 1, mean) %>% t()
                X <- S %*% a$A %>% t()
                X <- sapply(seq(nrow(X)), function(idx) {
                               X[idx,] + M[idx]
                          }) %>% t()

                score <- sapply(seq(nrow(X)), function(idx) {
                                x <- X[idx,] %>% tail(14)
                                v <- valid[idx,] %>% tail(14)
                                #plot_series(rbind(x,v))
                                stats_err(x,v)$err
                          })
                mean(score)
            }) %>% unlist()

plot(scores); mean(scores)
