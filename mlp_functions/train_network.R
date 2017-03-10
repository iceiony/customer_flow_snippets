train_network <- function(train, view, hidden, rate, duration){
    tic()
    errors <- numeric(duration)

    w <- init_weights(c(period + vperiod, hidden, 1))
    L <- length(train)

    for(epoch in 1:length(errors)){
        
        repeat{
            pred_len <- 14
            idx_start <- sample(L - period - vperiod, 1) + period + vperiod
            idx_end   <- min(L, idx_start + pred_len - 1)

            TARG <- train[idx_start:idx_end] 
            dim(TARG) <- c(length(TARG), 1)

            IN <- c()
            tr <- train[seq(idx_start - period, idx_start - 1)]
            for(i in seq(idx_start, idx_end)){
                vr <- view[seq(i - period - vperiod, i - period - 1)]
                IN <- rbind(IN, c(tr,vr))
                next_day <- run_network(c(tr,vr), w) %>% last()
                tr <- c(tr[-1], next_day)
            }

            IN[is.na(IN)] <- 0
            nas <- which(is.na(TARG))
            if(length(nas)>0){
                TARG <- TARG[-nas, , drop = F]
                IN   <- IN[-nas, , drop = F]
            }

            if(nrow(TARG)>0) break;
        }

        net_out <- run_network(IN, w) 
        if(any(is.na(last(net_out)))){
            stop('output is na')
        }

        errors[epoch] <- sse_cost(last(net_out), TARG)

        delta <- determine_delta(TARG, net_out, w)
        w <- update_weights(w, delta, rate)

        rate <- rate * 0.999
    }

    toc()
    errors[is.nan(errors)] <- NA
    errors[is.infinite(errors)] <- NA
    list(w=w,errors=errors[1:epoch-1])
}


