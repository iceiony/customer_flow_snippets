prepare <- function(signal, smoothing = 1){
    require('smoother')
    signal <- unlist(signal)
    signal <- signal[-seq(first(which(!is.na(signal))))]
    signal[is.na(signal)] <- mean(signal, na.rm = T)
    smth.gaussian(signal, window = smoothing, tails=T)
}
