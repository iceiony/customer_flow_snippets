source('./init.R')
source('./mlp_functions/init.R')
source('./fourier_functions/init.R')

shop_sales  <- read.table('./data/daily_shop_sales.csv', sep = ',', header = T)
signal <- unlist(shop_sales[9, -1])
signal <- signal[-seq(first(which(!is.na(signal))))]
signal[is.na(signal)] <- 0

IN <- TARG <- c()
phases <- list()
epochs <- 2
padding <- rep(0,0)
for(i in seq(epochs)){
    pi2 <- pi * 2
    len_train <- length(signal) - 14# ceiling(pi2 * (length(signal) %/% pi2 - 2) )
    y <- signal[seq(len_train)]
    TARG <- c(TARG, t(t(y)))

    y[sample(seq(len_train), len_train * 0.9)] <- 0 #add noise
#    y <- c(y, rep(0,100))             #padd to full period

#    zero_idx <- seq(len_train/10) + round(len_train/10 * ((i-1) %% 3))
#    zero_idx <- seq(80) + floor( (len_train - 80 + 16) * (i - 1) / 20 )
    lambda <- ( (epochs+1)/epochs * (i-1) )/epochs
    len_zero <- round(len_train * 0.15)
    zero_idx <- seq(len_zero) + round((len_train - len_zero) * lambda )
    y[zero_idx] <- 0

    y <- c(y,padding)
    components <- fft_components(y)
    reconst <- reconstruct(components, len_train)

    IN <- rbind(IN, reconst)
    phases[[i]] <- components$phase
}

phases <- do.call(rbind,phases)
y <- signal[seq(len_train)]
y <- c(y,padding)
components <- fft_components(y)
components$phases <- apply(phases, 2, mean)

net <- single_layer_train(IN, TARG)
IN <- reconstruct(components, length(signal))
res <- c(last(run_network(IN, net$w)))

plot(signal,, 'l')
lines(res, col='red', lwd = 3)

len_pred <- 14
res <- tail(res, len_pred)
signal <- tail(signal, len_pred)
pred_err <- error_cost(res, signal)
#plot(res,,'l', col='red', lwd = 3)
#lines(signal)
message('Prediction error : ', pred_err)
message('Correlation :', cor(res, signal))

