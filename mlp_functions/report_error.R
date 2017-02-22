report_error <- function(out, targ, len = 14){
    out  <- tail(out, len)
    targ <- tail(targ, len)
    err  <- error_cost(out, targ)
    message('Prediction error : ', err)
    message('Correlation :', cor(out, targ))
}
