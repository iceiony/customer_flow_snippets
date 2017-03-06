source('./init.R')
source('./mlp_functions/init.R')
shop_sales <- read.table('./data/daily_shop_sales.csv', sep = ',', header = T)
shop_trend <- read.table('./data/daily_shop_trend.csv', sep = ',', header = T)
shop_pred  <- read.table('./kieran\ alg/pred.csv', sep = ',', header = F)

colnames(shop_pred) <- c('shop_id', 1:14)
colnames(shop_trend) <- colnames(shop_sales)

retrended <- mclapply(seq(nrow(shop_pred)), mc.cores = 7,
             function(idx){
                message(idx)

                s <- shop_pred[idx,]
                m <- filter(shop_trend, shop_id == s$shop_id)

                if(nrow(m)==0) return(s) 

                v <- s + tail(unlist(m), 15) - mins[idx]
                v$shop_id <- s$shop_id
                return(v)
             }) %>% bind_rows()

retrended <- round(retrended)
retrended <- arrange(retrended, shop_id)
retrended[retrended<0] <- 0

scores <- mclapply(seq(nrow(retrended)), mc.cores = 7,
            function(idx){
                    message(idx)
                    x <- retrended[idx,] %>% unlist() %>% tail(14)
                    v <- shop_sales[idx,] %>% unlist() %>% tail(14)
                    #plot_series(rbind(x,v))
                    stats_err(x,v)$err
             }) %>% unlist()


