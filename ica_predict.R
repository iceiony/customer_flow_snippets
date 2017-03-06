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

prepare <- function(shop_sales, cls, trim = 0, ws = 0.00, sma = 1){
                group  <- (filter(shop_sales, cluster == cls)) %>% 
                            apply(1,function(x) {
                               x[is.na(x)] <- 0
                               x <- x[-c(1,length(x))] %>% 
                                   smth(method = 'sma', n = sma, tails=T) %>%
                                   smth.gaussian( ws, tails=T)

                               if(trim==0) return(x)
                               head(x, -trim)
                            }) %>% t() 
                group <- group[,-seq(0)]
                group[is.na(group)] <- 0
                group
            }

scores  <-  mclapply(sort(unique(shop_sales$cluster)), mc.cores = 7,
            function(cls){
                message(cls)
                ws  <- 0.25
                sma <- 3

                train <- prepare(shop_sales, cls, 14, ws, sma)
                valid <- prepare(shop_sales, cls, 0,  ws, sma)
                #plot_series(valid)

                a <- fastICA(t(train), nrow(train))
                S <- apply(a$S, 2, function(s){
                                #s  <- ts(s, frequency = length(s))
                                #reg <- fourier(s, K = 2)
                                #fit <- auto.arima(s, xreg = reg, seasonal = F) 
                                #fc  <- forecast(fit, 14, xreg = reg)
                                fit  <- auto.arima(s, seasonal = T)
                                fc  <- forecast(fit, 14)
                                c(s , head(fc$mean, 14))
                          })

                M <- apply(train, 1, mean) %>% t()
                X <- S %*% a$A %>% t()
                X <- sapply(seq(nrow(X)), function(idx) {
                               X[idx,] + M[idx]
                          }) %>% t()

                X[X<0] <- 0

                score <- sapply(seq(nrow(X)), function(idx) {
                                x <- X[idx,] %>% tail(14)
                                v <- valid[idx,] %>% tail(14)
                                #plot_series(rbind(x,v))
                                stats_err(x,v)$err
                          })
                mean(score)
            }) %>% unlist()

plot(scores); mean(scores)

all_scores <- append(all_scores, mean(scores))
