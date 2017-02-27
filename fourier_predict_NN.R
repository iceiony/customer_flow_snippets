source('./init.R')
source('./mlp_functions/init.R')
source('./fourier_functions/init.R')
library('smoother')

shop_sales  <- read.table('./data/daily_shop_sales.csv', sep = ',', header = T)

predictions <- mclapply(seq(nrow(shop_sales)), mc.cores = 8,
#predictions <- mclapply(rep(1556,100), mc.cores = 8,
                function(idx){
                    shop    <- shop_sales[idx,]
                    shop_id <- shop['shop_id']
                    message(shop_id)

                    best <- list(trend = c(), frst = c(), pred = c())
                    for(t in seq(10)){
                        signal <- prepare(shop, 7 * 7)# %>% head(-14)

                        trend <- signal %>%
                                 smth.gaussian(0.9, tails = T) %>%
                                 make_prediction(trended = F)

                        frst  <- trend

                        smoothings <- c(0.5, 0.1, 0.0)
                        for(i in smoothings){
                            pred <- (signal - head(trend, -14)) %>% 
                                     smth.gaussian(i, tails = T) %>%
                                     make_prediction(trended = F)
                            pred <- pred + trend   

                            if(i > 0) trend <- pred
                        }

                        first_cor <- cor(head(frst, -14), signal) * 10
                        trend_cor <- cor(head(trend, -14), signal) * 10
                        pred_cor  <- cor(head(pred, -14), signal) * 10

                        frst  <- tail(frst, 14)
                        trend <- tail(trend, 14)
                        pred  <- tail(pred, 14)

                        best$frst  <- rbind(best$frst, c(first_cor, frst))
                        best$trend <- rbind(best$trend, c(trend_cor, trend))
                        best$pred  <- rbind(best$pred, c(pred_cor, pred))
                    }

                    frst  <- with(best, apply(frst[,-1] * frst[,1], 2, sum)   / sum(frst[,1]))
                    trend <- with(best, apply(trend[,-1] * trend[,1], 2, sum) / sum(trend[,1]))
                    pred  <- with(best, apply(pred[,-1] * pred[,1], 2, sum)   / sum(pred[,1]))
                    shop     <- prepare(shop, 7 * 9)# %>% tail(14)

                    #plot(shop,,'l',col='red',lwd=3) 
                    #lines(frst,,lwd=2, col='green')
                    #lines(trend,,lwd=2, col='blue')
                    #lines(pred,,lwd=2);
                    #report_error(frst, shop, 14)
                    #report_error(trend, shop, 14)
                    #report_error(pred, shop, 14)

                    st_trend <- stats_err(trend, shop, 14) 
                    st_pred  <- stats_err(pred, shop, 14)
                    st_first <- stats_err(frst, shop, 14)

                    scores <- c(st_first$err, st_trend$err, st_pred$err)
                    data.frame( 
                               frst = I(list(frst)),
                               trend = I(list(trend)),
                               pred = I(list(pred)),
                               shop_id   = shop_id,
                               scores    = I(list(scores)), 
                               best_idx  = which.min(c(scores, 1)),
                               best_score  = min(c(scores, 1))
                               )
               }) 


p <- bind_rows(predictions)


#tmp <- lapply(seq(2000),
#          function(shop_id){
#              s <- strategy$best_idx[shop_id]
#              if(s == 1) return(c(shop_id, p$frst[[shop_id]]))
#              if(s == 2) return(c(shop_id, p$trend[[shop_id]]))
#              if(s == 3) return(c(shop_id, p$pred[[shop_id]]))
#           })
#
#tmp <- do.call(rbind, tmp)
#tmp[tmp <= 0]  <- 0 
#tmp <- round(tmp)
#
#write.table(tmp, 'pred4.csv', sep=',', row.names = F, col.names = F)
