determine_delta <- function(targ,out,w){
   delta <- as.list(rep(0,length(w)))
   dif <- targ - last(out)

   for(layer in length(w):1 ) {
       for(p in 1:length(targ)){
           delta[[layer]] <- delta[[layer]] + out[[layer]][p,] %o% dif[p,]
       }
   
       if(layer > 1){
           dif  <- t(tcrossprod(w[[layer]], dif)) * out[[layer]] * (1 - out[[layer]])
           dif <- dif[, -1]
       }
   }
   delta
}
