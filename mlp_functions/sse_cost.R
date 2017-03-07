sse_cost <- function(out, targ){
    sum((out-targ)^2) / 2
}
