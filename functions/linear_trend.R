linear_trend <- function(signal){
    #signal <- smth.gaussian(signal, window = 0.4, tails = T)
    #x <- seq_along(signal)
    #y <- signal[x]
    not_na <- first(which(!is.na(signal)))
    signal <- signal[-seq(not_na)]

    x <- c(1, length(signal))
    y <- c(mean(head(signal, 10), na.rm = T), mean(tail(signal),10, na.rm = T))

    trend <- lm(y ~ x)

    trend$remove <- function(signal){
         signal - predict(trend, newdata = list(x = seq_along(signal)))
    }

    trend$add    <- function(signal){
         signal + predict(trend, newdata = list(x = seq_along(signal)))
    }

    trend
}


null_trend <- function(){
    list(remove = identity,
         add    = identity)
}
