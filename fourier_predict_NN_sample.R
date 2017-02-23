source('./init.R')
source('./mlp_functions/init.R')
source('./fourier_functions/init.R')

generate_signal <- function(duration, f0){
    x <- seq(0, duration, f0)
    x <- x[-1]
    y <- 1.2 * sin(3 * x + pi/4) + 0.55 * sin(10 * x) + 2 
    data.frame(x,y)
}

period <- 2 * pi
f0     <- pi / 100
signal <- generate_signal(2.5 * period, f0)

IN <- TARG <- c()

for(i in seq(1)){
    y  <- signal$y
    Ly <- length(y) 
    TARG <- c(TARG, t(t(y)))

    y[sample(seq(Ly), Ly * 0.3)] <- 0 #add noise
    y <- c(y, rep(0,100))             #padd to full period

    components <- fft_components(y)
    reconst <- reconstruct(components, length(TARG))
    IN <- rbind(IN, cbind(reconst, components$a0))
}

net <- single_layer_train(IN, TARG)
IN <- reconstruct(components, length(signal$x) * 2)
IN <- cbind(IN, components$a0)
      
res <- last(run_network(IN, net$w))

signal <- generate_signal(length(res) * f0, f0)
plot(signal$x, res, 'l', col='red', lwd=3)
lines(signal$x, signal$y)
message('Prediction error : ', error_cost(res, signal$y))
