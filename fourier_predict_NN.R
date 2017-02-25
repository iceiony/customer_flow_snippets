source('./init.R')
source('./mlp_functions/init.R')
source('./fourier_functions/init.R')
library('smoother')

shop_sales  <- read.table('./data/daily_shop_sales.csv', sep = ',', header = T)

predictions <- mclapply(seq(nrow(shop_sales)), mc.cores = 9,
#predictions <- mclapply(rep(1556,100), mc.cores = 8,
                function(idx){
                    shop    <- shop_sales[idx,]
                    shop_id <- shop['shop_id']
                    message(shop_id)
                    shop    <- prepare(shop, 7 * 9)
                    signal  <- shop %>% head(-14)

                    trend <- signal %>%
                             smth.gaussian(0.9, tails = T) %>%
                             make_prediction(trended = F)

                    first_trend  <-  trend

                    smoothings <- c(0.5, 0.1, 0.0)
                    for(i in smoothings){
                        pred <- (signal - head(trend, -14)) %>% 
                                 smth.gaussian(i, tails = T) %>%
                                 make_prediction(trended = F)
                        pred <- pred + trend   

                        #plot(shop,,'l',col='red',lwd=3) 
                        #lines(trend,,lwd=2, col='blue')
                        #lines(pred,,lwd=2);
                        #report_error(trend, shop, 14)
                        #report_error(pred, shop, 14)

                        trend <- pred
                    }

                    st_trend <- stats_err(trend, shop, 14) 
                    st_pred  <- stats_err(pred, shop, 14)
                    st_first <- stats_err(first_trend, shop, 14)

                    data.frame( 
                               shop_id     = shop_id,
                               trend_score = min(st_trend$err,1),
                               trend_cor   = cor(trend, shop),
                               pred_score  = min(st_pred$err, 1),
                               pred_cor    = cor(pred, shop), 
                               first_score = min(st_first$err,1),
                               first_cor   = cor(first_trend, shop),
                               best_score  = min(st_trend$err, st_pred$err, st_first$err, 1) 
                               )
               }) 

predictions <- bind_rows(predictions)
