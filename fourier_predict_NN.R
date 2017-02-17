source('./init.R')
source('./mlp_functions/init.R')

period <- 2 * pi
f0 <- pi / 100
x <- seq(0, 1.5*period, pi/100)
x <- x[-1]
y <- 1.2 * sin(3 * x + pi/4) + 0.55 * sin(10 * x) + 2 

TARG <- t(t(y))
#add noise to the sequence
noise_idx <- sample(seq_along(y), length(y) * 0.3)
#noise_idx <- round(seq(3, length(y), 2.2))
y[noise_idx] <- 0
y <- c(y, rep(0,100))
#y[200:300]  <- 0
#y <- y[1:200]
plot(y,,'l')

fourier <- fft(y)

L <- length(fourier)

a0 <- Mod(fourier[1]) / L
fourier <- fourier[seq(2, L/2)] * 2 / L
ampli <- unname(Mod(fourier))
phase <- unname(Arg(fourier) + pi/2)
freq  <- seq_along(fourier) * (period/f0) / L

generate_in <- function(size){
    sapply(seq(size) - 1, 
         function(t){
             x <- freq * f0 * t
             y <- phase
             # sin(x+y) = sin(x)cos(y) + cos(x)sin(y)
             # sin(freq * f0 * t + phase)
             c(sin(x+y))
      }) %>% t() %>% cbind(a0)
}

IN <- generate_in(length(x))

IN <- cbind(1, IN)
net <- list(w = list(pseudoinverse(IN) %*% TARG))
#hidden <- c(100)
#net <- train_network(IN, TARG, hidden, c(.00019,.00014), 500)
#plot(net$errors, type = 'l', ylim = c(0, max(net$errors)), xlim=c(0,500))
#message('Training error : ' , last(net$errors))

#try predicting the future
IN <- generate_in(length(x) * 2)

res <- last(run_network(IN,net$w))
x <- seq(0, length(res) * pi/100, pi/100)
x <- x[-1]
y <- 1.2 * sin(3 * x + pi/4) + 0.55 * sin(10 * x) + 2 
res <- res
plot(x,res,'l',col='red',lwd=3)
lines(x,y)
message('Prediction error : ', error_cost(res, y))
