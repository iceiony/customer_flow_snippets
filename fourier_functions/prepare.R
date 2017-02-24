prepare <- function(shop_sales, id , duration){
#take duration from most recent of data
    signal <- filter(shop_sales, shop_id == id)
    signal <- unlist(signal) %>% tail(-1) %>% rev()

    duration <- min(length(signal), duration)

    signal <- signal[seq(duration)] %>% rev()
    signal[is.na(signal)] <- 0
    signal
}
