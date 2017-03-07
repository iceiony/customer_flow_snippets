error_cost <- function(out, targ){
    error <- abs((out - targ)/(out + targ))
    error[is.nan(error)] <- 0
    error[is.na(error)] <- 0
    error[error == Inf] <- 0
    sum(error)/length(targ)
}
