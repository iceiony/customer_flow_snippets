library('purrr')

period <- 2 * pi
f0 <- pi / 100
x <- seq(0, 3*period, pi/100)
x <- x[-1]
y <- 1.2 * sin(3 * x + pi/4) + 0.55 * sin(10 * x) + 2 

trend <- lm(y~x)
fourier <- fft(trend$residuals)
duration <- 3
sample_freq <- y

L <- length(fourier)

a0 <- Mod(fourier[1]) / L
fourier <- fourier[seq(2, L/2)] * 2 / L
ampli <- unname(Mod(fourier))
phase <- unname(Arg(fourier) + pi/2)
freq  <- seq_along(fourier) * (period/f0) / L

imp <- order(-ampli)
imp <- imp[seq(20)]
ampli <- ampli[imp]
phase <- phase[imp]
freq <- freq[imp]


res <- sapply(seq(L*2) - 1, function(t){
   a0 + sum(ampli * sin(freq * f0 * t + phase))
})
plot(res,,'l',col='red',lwd=3)
lines(trend$residuals,,'l')

#try predicting the future
x <- seq(0, 6*period, pi/100)
x <- x[-1]
y <- 1.2 * sin(3 * x + pi/4) + 0.55 * sin(10 * x) + 2 
res <- res + predict(trend, newdata = list(x))
plot(x,res,'l',col='red',lwd=3)
lines(x,y)
abline(trend,,col='blue')
