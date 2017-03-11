normalise <- normalize <- function(signal, trended = F){
    trnd_ <- 0

    if(trended){
        sig <- signal
        sig[is.na(sig)] <- 0
        trnd_ <- trend(sig, 0.03)
    }

    signal <- signal - trnd_ 

    mean_ <- mean(signal, na.rm = T)
    norm_ <- max(signal, na.rm = T) - min(signal, na.rm = T)

    out <- (signal - mean_) / norm_ 
    attr(out, 'mean') <- mean_
    attr(out, 'norm') <- norm_
    attr(out, 'trnd') <- trnd_

    out
}

denormalise <- denormalize <- function(signal, 
                                       attrib = attributes(signal),
                                       trended = F){
    if(trended){
        require('forecast')
        fit <- auto.arima(attrib$trnd, seasonal = F)
        fc <- forecast(fit, length(signal))
        attrib$trnd <- head(fc$mean, length(signal))
    }

    signal * attrib$norm + attrib$mean + attrib$trnd
}
