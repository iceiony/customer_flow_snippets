library('Rmisc')

scores <- mclapply(seq(24), mc.cores = 8,
             function(shop_id){
                error <- source('mlp_predict.R', local = T)
                error$value
             }) %>% bind_rows()

all_scores <- cbind(all_scores, scores$err)

#meanvar.plot(all_scores)
mn <- apply(all_scores, 2, min) %>% melt()
mn$Var2 <- seq(nrow(mn))

su <- summarySE(melt(all_scores), measurevar='value', groupvar=c('Var2'))
ggplot(su, aes(x = Var2, y = value)) +
    geom_errorbar(aes(ymin = value - sd, ymax = value + sd)) + 
    geom_line() + 
    geom_point(data = mn)

