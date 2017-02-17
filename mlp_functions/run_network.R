run_network <- function(net_in, w){
    net_out <- accumulate(w, .init = cbind(1,net_in), 
       function(out, w){
           cbind(1,sigmoid(out %*% w))
    })

    #remove bias from last output
    last_out <- last(net_out)[,-1]
    dim(last_out) <- c(nrow(net_in),1)
    net_out[[length(net_out)]] <- last_out

    net_out
}
