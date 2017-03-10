normalise <- normalize <- function(signal){
    mn <- min(signal, na.rm = T)
    mx <- max(signal, na.rm = T)

    out <- (signal - mn)/(mx - mn)
    attr(out, 'min')  <- mn
    attr(out, 'norm') <- mx - mn

    out
}

denormalise <- denormalize <- function(signal, attrib = attributes(s)[c('min','norm')]){
    signal * attrib$norm + attrib$min
}
