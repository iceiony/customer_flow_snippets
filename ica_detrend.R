source('./init.R')
shop_sales <- read.table('./data/daily_shop_sales.csv', sep = ',', header = T)
shop_trend <- read.table('./data/daily_shop_trend.csv', sep = ',', header = T)

colnames(shop_trend) <- colnames(shop_sales)

detrended <- mclapply(seq(nrow(shop_sales)), mc.cores = 7,
             function(idx){
                message(idx)

                s <- shop_sales[idx,]
                m <- filter(shop_trend, shop_id == s$shop_id)

                if(nrow(m)==0) return(s) 

                v <- s - m
                v$shop_id <- s$shop_id
                return(v)
             }) %>% bind_rows()

mins <- mclapply(seq(nrow(detrended)), mc.cores = 7,
          function(idx){
              m <- min(detrended[idx,-1], na.rm=T)
              if(m < 0) return(abs(m) + 10)
              return(0)
          }) %>% unlist()

d <- mclapply(seq(nrow(detrended)), mc.cores = 7,
                function(idx){
                    shop_id <- detrended[idx,]$shop_id
                    sales <- detrended[idx,-1] + mins[idx] 
                    c(shop_id = shop_id, unlist(sales))
                })

detrended <- do.call(rbind, d) %>% as.data.frame()

detrended <- round(detrended)
detrended <- arrange(detrended, shop_id)

write.table(detrended,'./data/daily_shop_detrended.csv', row.names = F, sep=',', na = '')
