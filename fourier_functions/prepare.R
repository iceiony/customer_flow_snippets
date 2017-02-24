prepare <- function(shop_sales, duration, id = shop_sales$shop_id){
#take duration from most recent of data
    signal <- filter(shop_sales, shop_id == id)
    signal <- unlist(signal) %>% tail(-1) %>% rev()
    duration <- min(length(signal), duration)
    signal <- signal[seq(duration)] %>% rev()

    repeat{
        if(!any(is.na(signal))) break
        st <- first(which(is.na(signal)))
        nd <- st + first(which(!is.na(signal[-seq(st)])))
        
        if(st == 1) {
            signal[st:nd] <- signal[nd]
            next
        }

        st <- st - 1
        signal[st:nd] <- seq(signal[st],signal[nd], length.out = nd-st+1)

    }

    signal[is.na(signal)] <- mean(signal, na.rm=T) 
    
    signal
}
