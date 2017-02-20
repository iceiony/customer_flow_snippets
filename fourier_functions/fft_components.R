fft_components <- function(series){
#returns the individual amplitude, phase and frequency components of the domain
    fourier <- fft(series)
    L <- length(fourier)

    f0 <- 1 / L
    a0 = Mod(fourier[1]) / L
    fourier <- fourier[seq(2, L)] / L

    list(
        f0 = f0,
        a0 = a0,
        ampli = unname(Mod(fourier)),
        phase = unname(Arg(fourier) + pi/2),
        freq  = seq_along(fourier) * f0
    )

}
