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
        
        fill_nd <- mean(signal[nd + 0:2])
        if(st == 1) {
            signal[st:nd] <- fill_nd
            next
        }

        st <- st - 1
        fill_st <- mean(signal[st - 0:2])
        signal[st:nd] <- seq(fill_st, fill_nd, length.out = nd-st+1)

    }

    signal[is.na(signal)] <- mean(signal, na.rm=T) 
    
    signal
}
