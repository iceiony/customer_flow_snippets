library('Rmisc')
library('grid')
source('./mlp_predict.R')

shop_sales <- read.table('./data/daily_shop_sales.csv', sep = ',', header = T)

mlp_ensemble <- function(params, shop_id){
    max_ensembles <- if(params$trended) 10 else 63

    nets <- mclapply(seq(max_ensembles), mc.cores = 7,
                 function(idx){
                    sales <- unlist(shop_sales[shop_id, -1])# %>% head(-14)

                    chop <- sample(17, 1) - 1
                    if(chop > 0) sales <- head(sales, -chop)
                    
                    chop <- sample(17, 1) - 1
                    if(chop > 0) sales <- tail(sales, -chop)
                    
                    valid <- tail(sales, 7) 
                    sales <- head(sales, -7)

                    net <- mlp_train(sales, params)
                    net$shop_id <- shop_id
                    #message('Train error: ', net$train_error)

                    pred  <- mlp_predict(sales, params, net, 7)
                    net$score <- stats_err(pred, valid)

                    return(net)
                 }) 

    predictions <- ldply(nets, function(net){
                        sales <- unlist(shop_sales[net$shop_id, -1])
                        valid <- tail(sales, 14)

                        pred  <- mlp_predict(sales, params, net, 14)
                        error <- stats_err(pred, valid)$err

                        data.frame(error, data = I(list(pred)))
                  })

    pred_w <- laply(nets, function(net){ 
                    if(is.na(net$score$cor)) return(1)
                    if(net$score$cor < 0.3) return(0)
                    if(abs(net$score$err) > 0.12) return(0)
                    return(net$score$cor)
              })

    #if(sum(pred_w>0) == 0 && params$trend) pred_w[] <- 1
    #if(sum(pred_w>0) == 0) return(Inf)
    if(sum(pred_w>0) == 0) pred_w[] <- 1

    avg_pred <- laply(predictions$data, as.numeric) %>%
                apply(2, function(pred){
                    sum(pred * pred_w) / sum(pred_w)
                })

    return(avg_pred)
    #print('Avg error')
    #valid <- unlist(shop_sales[shop_id, -1]) %>% tail(14)
    #score <- report_error(avg_pred, valid)$err

    #return(score)
}
