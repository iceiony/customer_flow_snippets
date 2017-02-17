run_network <- function(net_in, w){
    net_out <- list(cbind(1, net_in))
    for(i in seq_len(length(w) - 1)){
        net_out[[i + 1]] <- cbind(1, sigmoid(last(net_out) %*% w[[i]]))
    }

    i <- length(net_out) 
    net_out[[i + 1]] <- last(net_out) %*% w[[i]]
    net_out
}
