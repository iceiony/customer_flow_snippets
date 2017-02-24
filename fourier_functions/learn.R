learn <- function(signal, pad = 0){
    signal <- if(pad < 0) head(signal, pad) else c(signal,rep(0, pad))  

    epochs <- 10
    IN <- phase <-  c()
    for(i in seq(epochs)){
        components <- signal %>% 
                      add_noise(i, epochs, 0.1, 0.1) %>%
                      fft_components()
       
        phase  <- rbind(phase, components$phase)
        IN     <- rbind(IN, reconstruct(components))
    }

    TARG <- rep(signal, epochs) %>% t() %>% t()
    net <- single_layer_train(IN, TARG)

    components$phases <- apply(phase, 2, mean)
    net$components <- components

    class(net) <- 'fourier'
    net
}

add_noise <- function(signal,i, epochs, randomed, windowed){
#poultes the signal with a moving window of zeros and uniformly distributed zeros
    len <- length(signal)

    idx <- sample(seq(len), len * randomed)
    signal[idx] <- 0

    lambda <- (epochs + 1) * (i - 1) / epochs^2
    len_0  <- round(len * windowed)
    idx    <- seq(len_0) + (len - len_0) * (1 - lambda)
    signal[idx] <- 0

    signal
}

