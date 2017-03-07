normalise <- normalize <- function(signal){
    out <- (signal - min(signal))/(max(signal) - min(signal))
    attr(out, 'min') <- min(signal)
    attr(out, 'norm') <- max(signal) - min(signal)

    out
}

denormalise <- denormalize <- function(signal){
    signal * attr(signal, 'norm') + attr(signal,'min')
}
