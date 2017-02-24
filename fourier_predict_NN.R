source('./init.R')
source('./mlp_functions/init.R')
source('./fourier_functions/init.R')
library('smoother')

shop_sales  <- read.table('./data/daily_shop_sales.csv', sep = ',', header = T)

predictions <- shop_sales %>% 
               rowwise()  %>% 
               lapply(#mc.cores = 8,
                function(shop){
#                    shop <- shop_sales[4,]
                    shop_id <- shop$shop_id
                    shop    <- prepare(shop, 7 * 9)
                    signal  <- shop %>% head(-14)

                    trend <- signal %>%
                             smth.gaussian(0.9, tails = T) %>%
                             make_prediction(trended = F)

                    smoothings <- c(0.5,0.1,0.0)
                    for(i in smoothings){
                        pred <- (signal - head(trend, -14)) %>% 
                                 smth.gaussian(i, tails = T) %>%
                                 make_prediction(trended = F)
                        pred <- pred + trend   

                        plot(shop,,'l',col='red',lwd=3); 
                        lines(trend,,lwd=2, col='blue');
                        lines(pred,,lwd=2);
                        report_error(trend, shop, 14)
                        report_error(pred, shop, 14)
                        trend <- pred
                    }
               })


