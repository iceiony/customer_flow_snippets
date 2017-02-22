linear_trend <- function(signal, trim = 0){
    x <- c(1,length(signal) + trim)
    y <- signal[x]

    trend <- lm(y ~ x)

    trend$remove <- function(signal){
         signal - predict(trend, newdata = list(x = seq_along(signal)))
    }

    trend$add    <- function(signal){
         signal + predict(trend, newdata = list(x = seq_along(signal)))
    }

    trend
}
