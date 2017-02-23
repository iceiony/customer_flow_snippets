linear_trend <- function(signal, trim = 0){
    #x <- c(1,length(signal) + trim)
    #y <- signal[x]
    x <- seq_along(signal)
    y <- signal[x] %>% smth.gaussian(window=0.1, tails = T)

    trend <- lm(y ~ x)

    trend$remove <- function(signal){
         signal - predict(trend, newdata = list(x = seq_along(signal)))
    }

    trend$add    <- function(signal){
         signal + predict(trend, newdata = list(x = seq_along(signal)))
    }

    trend
}
