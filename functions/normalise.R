normalise <- normalize <- function(signal){
    mean_ <- mean(signal, na.rm = T)
    norm_ <- max(signal, na.rm = T) - min(signal, na.rm = T)

    out <- (signal - mean_) / norm_ 
    attr(out, 'mean') <- mean_
    attr(out, 'norm') <- norm_

    out
}

denormalise <- denormalize <- function(signal, attrib = attributes(signal)){
    signal * attrib$norm + attrib$mean
}
