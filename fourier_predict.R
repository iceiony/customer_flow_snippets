library('purrr')
timeseries <- function(fourier, duration, sample_freq){
    i <- complex(real = 0, imaginary = 1) #making sure i is complex

    L <- length(fourier)
    #moments <- reduce(seq(duration %/% L + 1), .init = 0,
    #              function(m, p){
    #                  c(m , seq(last(m), L * p - 1)) 
    #              })
    #moments <- moments[seq(2,duration)]
    #moments <- c(seq(L)-1,seq(L,L*2))
    moments <- seq(duration) -1
    message(last(moments))

    base_freq <- sample_freq / L
    a0 <- Mod(fourier[1]) / L
    fourier <- fourier[seq(2, L)] / L
    ampli <- Mod(fourier)
    phase <- Arg(fourier)
    freq  <- (seq_along(fourier)) * base_freq

    res <- sapply(moments, function(t){
       a0 + sum(ampli * cos(freq * 2*pi * t + phase))
    })
}

x <- seq(0,2*pi ,pi/100)
y <- 1.2 * sin(3 * x + pi/2) + 0.55 * sin(10 * x) + 2 
trend <- lm(y ~ x)

fourier <- fft(y)
res <- timeseries(fourier, length(x), 1)

plot(res,,'l',col='red',lwd=3)
lines(y,,'l')
