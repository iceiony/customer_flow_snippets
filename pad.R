library('Rmisc')
library('grid')

nets <- mclapply(seq(24), mc.cores = 8,
             function(idx){
                error <- source('mlp_predict.R', local = T)
                c(score = error$value$err, I(net))
             }) 


v <- shop_views[shop_id, -1] %>% prepare()
s <- shop_sales[shop_id, -1] %>% prepare(length(v))
q <- mean(v, na.rm = T)#trend(v, 0.25)
r <- mean(s, na.rm = T)#trend(s, 0.25)
v <- (v - q)
s <- (s - r)
valid <- tail(s + r, 14)
attributes(valid) <-  attributes(s)[c('min','norm')]
valid <- (valid / 10) %>% denormalise()

preds <- ldply(nets, function(net){
                inn  <- head(s, -14) %>% tail(period)
                view <- head(v, -14) %>% tail(period + vperiod)
                pred <- predict_future(inn, view, net, 14)
                pred <- pred + tail(r, 14)
                attributes(pred) <- attributes(s)[c('min','norm')]
                pred  <- (pred  / 10) %>% denormalise()
                pred
           })

scores <- apply(preds, 1, function(pred){
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

