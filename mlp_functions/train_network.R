train_network <- function(IN, TARG, hidden, rate, duration){
    tic()
    errors <- numeric(duration)

    w <- init_weights(c(ncol(IN), hidden, ncol(TARG)))

    for(epoch in 1:length(errors)){
        batch <- sample(nrow(IN), 100, replace = T) 
        #batch <- seq(nrow(IN)) 

        net_out <- run_network(IN[batch,], w) 

        errors[epoch] <- sse_cost(last(net_out), TARG[batch,])

        delta <- determine_delta(TARG[batch,], net_out, w)
        w <- update_weights(w, delta, rate)

        rate <- rate * 0.995
    }

    toc()
    errors[is.nan(errors)] <- NA
    errors[is.infinite(errors)] <- NA
    list(w=w,errors=errors[1:epoch-1])
}
