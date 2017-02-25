report_error <- function(out, targ, len = 14){
    stats <- stats_err(out, targ, len)
    message('Prediction error : ', stats$err)
    invisible(stats)
}
