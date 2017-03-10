train_network <- function(views, sales, pre_views, pre_sales, hidden, rate, duration){
    tic()
    errors <- numeric(duration)

    w <- init_weights(c(pre_views, hidden, 1))
    L <- length(sales)

    for(epoch in 1:length(errors)){
        
        repeat{
            pred_len <- 14
            idx_start <- sample(L - pre_views, 1) + pre_views
            idx_end   <- min(L, idx_start + pred_len - 1)

            TARG <- sales[idx_start:idx_end] 
            dim(TARG) <- c(length(TARG), 1)

            IN <- c()
            sr <- sales[seq(idx_start - pre_sales, idx_start - 1)]
            for(i in seq(idx_start, idx_end)){
                vr <- views[seq(i - pre_views, i - pre_sales - 1)]
                IN <- rbind(IN, c(vr, sr))
                next_day <- run_network(c(vr, sr), w) %>% last()
                sr <- c(sr[-1], next_day)
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
        #errors[epoch]
        #plot_series(cbind(last(net_out),TARG) %>% t())

        delta <- determine_delta(TARG, net_out, w)
        w <- update_weights(w, delta, rate)

        rate <- rate * 0.999
    }

    toc()
    errors[is.nan(errors)] <- NA
    errors[is.infinite(errors)] <- NA
    list(w=w,errors=errors[1:epoch-1])
}


