stats_err <- function(out, targ, len = 14){
    out  <- tail(out, len)
    targ <- tail(targ, len)

    er <- error_cost(out, targ)
    co <- cor(out, targ, use = 'pairwise.complete.obs')

    list(err = er, cor = co)}
