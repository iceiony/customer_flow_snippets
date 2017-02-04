as.matrix2 <- function(series, len = ncol(series), process = identity){
   dat <- as.matrix(series[,-1])  
   dat <- padzeros(dat, len - ncol(series), side = 'left')
   dat[dat <= 0] <- NA
   dat <- process(dat)

   cbind(shop_id = series$shop_id, dat) 
}
