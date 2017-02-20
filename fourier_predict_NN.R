source('./init.R')
source('./mlp_functions/init.R')
source('./fourier_functions/init.R')

shop_sales  <- read.table('./data/daily_shop_sales.csv', sep = ',', header = T)
signal <- unlist(shop_sales[1, -1])
signal <- signal[-seq(first(which(!is.na(signal))))]
signal[is.na(signal)] <- 0

IN <- TARG <- c()

for(i in seq(1)){
    y  <- signal
    pi2 <- pi * 2
    y <- y[seq(pi2 * Ly %/% pi2)]
    Ly <- length(y) 
    TARG <- c(TARG, t(t(y)))

#    y[sample(seq(Ly), Ly * 0.3)] <- 0 #add noise
#    y <- c(y, rep(0,100))             #padd to full period

    components <- fft_components(y)
    reconst <- reconstruct(components, length(y))

    IN <- rbind(IN, cbind(reconst, components$a0))
}

net <- single_layer_train(IN, TARG)
#IN <- reconstruct(components, length(signal))
#IN <- cbind(IN, components$a0)
      
res <- c(last(run_network(IN, net$w)))
plot_series(rbind(y, res))

#signal <- generate_signal(length(res) * f0, f0)
#plot(signal$x, res, 'l', col='red', lwd=3)
#lines(signal$x, signal$y)
#message('Prediction error : ', error_cost(res, signal$y))
