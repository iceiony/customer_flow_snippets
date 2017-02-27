learn <- function(signal, pad_size = 0){
    epochs <- 50
    IN <- phase <-  c()
    for(i in seq(epochs)){
        components <- signal %>% 
                      pad(pad_size) %>%
                      add_noise(i, epochs, 0.1, 0.02) %>%
                      fft_components()
       
        phase <- rbind(phase, components$phase)
        recon <- reconstruct(components, length(signal)) 
        IN    <- rbind(IN, recon) 
    }

    TARG <- rep(signal, epochs) %>% t() %>% t()
    TARG <- TARG * 1
    net <- single_layer_train(IN, TARG)

    components$phases <- apply(phase, 2, mean)
    net$components <- components

    class(net) <- 'fourier'
    net
}

add_noise <- function(signal,i, epochs, randomed, windowed){
#poultes the signal with a moving window of zeros and uniformly distributed zeros
    len <- length(signal)

    if(randomed > 0){
        idx <- sample(seq(len), len * randomed)
        signal[idx] <- 0
    }

    if(windowed > 0){
        lambda <- (epochs + 1) * (i - 1) / epochs^2
        len_0  <- round(len * windowed)
        idx    <- seq(len_0) + (len - len_0) * (1 - lambda)
        signal[idx] <- 0
    }

    signal
}

