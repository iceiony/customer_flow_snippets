linear_trend <- function(signal){
    #signal <- smth.gaussian(signal, window = 0.4, tails = T)
    #x <- seq_along(signal)
    #y <- signal[x]
    x <- c(1, length(signal))
    y <- c(mean(head(signal, 10)), mean(tail(signal),10))

    trend <- lm(y ~ x)

    trend$remove <- function(signal){
         signal - predict(trend, newdata = list(x = seq_along(signal)))
    }

    trend$add    <- function(signal){
         signal + predict(trend, newdata = list(x = seq_along(signal)))
    }

    trend
}
