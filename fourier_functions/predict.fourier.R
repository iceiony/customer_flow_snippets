predict.fourier <- function(net, duration){
    IN <- reconstruct(net$components, duration)
    OUT <- run_network(IN, net$w)

    last(OUT)
}
