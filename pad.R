library('Rmisc')
library('grid')
source('./mlp_predict.R')

shop_sales <- read.table('./data/daily_shop_sales.csv', sep = ',', header = T)
pre_sales <- 16
shop_id <- 266 #sample(nrow(shop_sales), 1)

nets <- mclapply(seq(63), mc.cores = 7,
             function(idx){
                sales <- unlist(shop_sales[shop_id, -1])

                chop <- sample(17, 1) - 1
                if(chop > 0) sales <- head(sales, -chop)
                
                chop <- sample(17, 1) - 1
                if(chop > 0) sales <- tail(sales, -chop)
                
                valid <- tail(sales, 14) 
                sales <- head(sales, -14)

                net <- mlp_train(sales, pre_sales)
                net$shop_id <- shop_id
                #plot(net$errors, type = 'l', ylim = c(0, 100) , xlim=c(0, 1000))
                message('Train error: ', net$train_error)

                pred  <- mlp_predict(sales, pre_sales, net, 14)
                net$score <- stats_err(pred, valid)
                #plot_series(rbind(pred, valid))
                #report_error(pred,valid)

                return(net)
             }) 


predictions <- ldply(nets, function(net){
                    sales <- unlist(shop_sales[net$shop_id, -1])
                    valid <- tail(sales, 14)

                    pred  <- mlp_predict(sales, pre_sales, net, 14)
                    error <- stats_err(pred, valid)$err

                    #dev.hold()
                    #plot_series(rbind(pred, valid))
                    #dev.flush()

                    data.frame(error, data = I(list(pred)))
              })

scores <- predictions$error
dim(scores) <- c(length(scores), 1)

pred_w <- laply(nets, function(net){ 
                if(abs(net$score$cor) < 0.3) return(0)
                if(abs(net$score$err) > 0.12) return(0)
                return(net$score$cor)
          })
avg_pred <- laply(predictions$data, identity) %>%
            apply(2, function(pred){
                sum(pred * pred_w) / sum(pred_w)
                #mean(pred)
            })

print('Avg error')
sales <- unlist(shop_sales[shop_id, -1])
valid <- tail(sales, 14)
avg_err <- report_error(avg_pred, valid)$err

#all_avg <- all_scores <- c()

all_avg <- c(all_avg, avg_err)
all_scores <- cbind(all_scores, scores)

mn <- all_avg %>% melt()
mn$Var2 <- seq(nrow(mn))
su <- summarySE(melt(all_scores), measurevar='value', groupvar=c('Var2'))


p1 <- plot_series(rbind(avg_pred, valid)) 
p2 <- ggplot(su, aes(x = Var2, y = value)) +
        geom_errorbar(aes(ymin = value - se, ymax = value + se)) + 
        geom_line() + 
        geom_point(data = mn)

multiplot(p1,p2)
