train_network <- function(IN, TARG, hidden, rate, duration){
    errors <- numeric(duration)

    w <- init_weights(c(ncol(IN), hidden, ncol(TARG)))

    for(epoch in 1:length(errors)){
        net_out <- run_network(IN, w) 

        errors[epoch] <- entropy_cost(TARG, last(net_out))

        delta <- determine_delta(TARG, net_out, w)
        w <- update_weights(w, delta, rate)
    }

    list(w=w,errors=errors[1:epoch-1])
}
