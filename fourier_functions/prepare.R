prepare <- function(shop, duration){
#take duration from most recent of data
    signal <- unlist(shop) %>% tail(-1) 

    not_na <- first(which(!is.na(signal)))
    signal <- signal[-seq(not_na)] %>% rev() 
    duration <- min(length(signal), duration)
    signal   <- signal[seq(duration)] %>% rev()

    #remove outlisers
    trend  <- linear_trend(signal)
    signal <- signal %>% trend$remove()
    signal_mean <- mean(signal, na.rm = T)
    signal_sd   <- sd(signal, na.rm = T)
    outliers <- which(abs(signal - signal_mean) >= 2 * signal_sd)
    outliers <- Filter(f(x, signal[x] < signal_mean), outliers) 
    signal[outliers] <- NA
    signal <- signal %>% trend$add()

    repeat{
        if(!any(is.na(signal))) break
        st <- first(which(is.na(signal)))
        nd <- st + first(which(!is.na(signal[-seq(st)])))
        
        fill_nd <- mean(signal[nd + 0:2], na.rm = T)
        if(st <= 2) {
            signal[st:nd] <- fill_nd
            next
        }

        st <- st - 1
        fill_st <- mean(signal[st - 0:2], na.rm = T)

        if(is.na(nd) || nd == length(signal)){
            signal[-seq(st)] <- fill_st
            next
        }

        signal[st:nd] <- seq(fill_st, fill_nd, length.out = nd-st+1)

    }
    
    signal
}
