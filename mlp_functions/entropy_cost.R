entropy_cost <- function(targ,out){
    error <- sum( -sum(targ * log(out) + (1-targ) * log(1-out))) 
    error
}
