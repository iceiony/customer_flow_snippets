source('./init.R')
source('./mlp_ensemble.R')
library('R.utils')

#opt_params <- list()
#all_scores <- rep(Inf, 2000)
#for(shop_id in seq(1310, 2000)){
#    print(shop_id)
#    params <- list(
#                   pre_sales = 14,
#                   hidd = c(150, 50),
#                   rate = c(5e-2, 1e-2, 1e-2),
#                   train_len = 35,
#                   trended = F
#              )
#
#    scores <- rep(Inf, 3)
#    for(i in seq_along(scores)){
#        scores[i] <- mlp_ensemble(params, shop_id)
#        if(is.infinite(scores[i])) break
#    }
#
#    params$score    <- mean(scores)
#    params$score_sd <- sd(scores) 
#    params$shop_id  <- shop_id
#    opt_params[[shop_id]] <- params
#    all_scores[shop_id] <- params$score
#}

#for(shop_id in seq(1557,2000)){
#    if(!is.infinite(all_scores[shop_id]) || all_scores[shop_id] < 0.2) next
#    print(c('Trended :',shop_id))
#
#    params <- list(
#                   pre_sales = 20,
#                   hidd = c(150, 50),
#                   rate = c(5e-2, 1e-2, 1e-2),
#                   train_len = 25,
#                   trended = T
#              )
#
#    params$score  <- Inf
#    params$score  <- evalWithTimeout(mlp_ensemble(params, shop_id), timeout = 30, onTimeout = 'silent')
#    
#    params$shop_id  <- shop_id
#    params$score_sd <- NA
#    opt_params[[shop_id]] <- params
#    all_scores[shop_id] <- params$score
#}
#
#all_scores[is.na(all_scores)] <- Inf
#for(shop_id in seq(1, 2000)){
#    if(!is.infinite(all_scores[shop_id]) && all_scores[shop_id] < 0.25) next
#    print(c('Altered :',shop_id))
#
#    params <- list(
#                   pre_sales = 14,
#                   hidd = c(150, 50),
#                   rate = c(5e-2, 1e-2, 1e-2),
#                   train_len = 35,
#                   trended = F
#              )
#    params$score    <- mlp_ensemble(params, shop_id)
#    params$shop_id  <- shop_id
#    params$score_sd <- NA
#    if(all_scores[shop_id] > params$score){
#        opt_params[[shop_id]] <- params
#        all_scores[shop_id] <- params$score
#    }
#}

preds <- list()
for(shop_id in seq(shop_id,2000)){
    print(shop_id)
    params <- opt_params[[shop_id]]

    if(params$score > 0.2) next

    pred <- mlp_ensemble(params, shop_id) 
    preds[[shop_id]] <- c(shop_id, pred) 
}

pred_table <- do.call(rbind, preds)
preds <- round(pred_table)
preds[preds < 0] <- 0

old_pred <- read.table('./pred5.csv', sep=',', header = F)
miss <- !(1:2000) %in% preds[,1]
for(shop_id in miss){
    preds <- rbind(preds, old_pred[shop_id,])
}

write.table(pred_table,'./pred_last.csv', sep=',', row.names=F, col.names = F)
