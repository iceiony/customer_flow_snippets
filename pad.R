library('Rmisc')
library('grid')
source('./mlp_predict.R')

shop_sales <- read.table('./data/daily_shop_sales.csv', sep = ',', header = T)
shop_views <- read.table('./data/daily_shop_view.csv' , sep = ',', header = T)
pre_sales <- 20
pre_views <- pre_sales + 20

nets <- mclapply(seq(24), mc.cores = 8,
             function(idx){
                shop_id <- 3
                
                views <- unlist(shop_views[shop_id, -1])
                sales <- unlist(shop_sales[shop_id, -1])

                chop <- sample(21, 1) - 1
                if(chop > 0){
                    views <- head(views, -chop)
                    sales <- head(sales, -chop)
                }

                net <- mlp_train(views, sales, pre_views, pre_sales)
                net$shop_id <- shop_id

                plot(net$errors, type = 'l', ylim = c(0, 100) , xlim=c(0, 1000))
                net$train_error

                valid <- tail(sales, 14) 
                views <- head(views, -14)
                sales <- head(sales, -14)

                pred  <- mlp_predict(views, sales, pre_views, pre_sales, net, 14)

                plot_series(rbind(pred, valid))
                report_error(pred,valid)

                return(net)
             }) 

scores <- ldply(nets, function(net){
                views <- shop_views[net$shop_id, -1]
                sales <- shop_sales[net$shop_id, -1]
                pred  <- mlp_predict(views, sales, pre_views, pre_sales, net)

                pred  <- tail(pred, 14)
                valid <- tail(views, 14)

                stats_err(unlist(pred), valid)$err
          })
dim(scores) <- c(length(scores), 1)

avg_pred <- apply(preds, 2, function(pred){
                mean(pred)
          })

print('Avg error')
report_error(avg_pred, valid)
plot_series(rbind(avg_pred, valid))

all_scores <- cbind(all_scores, scores)
mn <- apply(all_scores, 2, min) %>% melt()
mn$Var2 <- seq(nrow(mn))
su <- summarySE(melt(all_scores), measurevar='value', groupvar=c('Var2'))
ggplot(su, aes(x = Var2, y = value)) +
    geom_errorbar(aes(ymin = value - sd, ymax = value + sd)) + 
    geom_line() + 
    geom_point(data = mn)

