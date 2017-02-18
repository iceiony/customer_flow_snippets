fft_components <- function(series, period, f0){
#returns the individual amplitude, phase and frequency components of the domain
    fourier <- fft(series)
    L <- length(fourier)
    a0 = Mod(fourier[1]) / L
    fourier <- fourier[seq(2, L/2)] * 2 / L

    list(
        period = period,
        f0 = f0,
        a0 = a0,
        ampli = unname(Mod(fourier)),
        phase = unname(Arg(fourier) + pi/2),
        freq  = seq_along(fourier) * (period/f0) / L
    )

}
