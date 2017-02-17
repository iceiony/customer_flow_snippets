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

IN <- sapply(seq(length(x))-1, 
         function(t){
             sin(freq * f0 * t + phase)
      }) %>% t()
IN <- cbind(IN,a0)

hidden <- c()
net <- train_network(IN, TARG, hidden, c(.0012), 500)

plot(net$errors, type = 'l', ylim = c(0, max(net$errors)), xlim=c(0,500))
message('Training error : ' , last(net$errors))

#try predicting the future
IN <- sapply(seq(length(x) * 2) - 1, 
         function(t){
             sin(freq * f0 * t + phase)
      }) %>% t()
IN <- cbind(IN,a0)

res <- last(run_network(IN,net$w))
x <- seq(0, length(res) * pi/100, pi/100)
x <- x[-1]
y <- 1.2 * sin(3 * x + pi/4) + 0.55 * sin(10 * x) + 2 
res <- res
plot(x,res,'l',col='red',lwd=3)
lines(x,y)
message('Prediction error : ', error_cost(res, y))
