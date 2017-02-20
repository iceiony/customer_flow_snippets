reconstruct <- function(components, duration){
#reconstructs harmonics for each frequency withouth using amplitudes 
    list2env(components, environment())
    sapply(seq(duration) - 1, 
         function(t){
             x <- freq * 2*pi * t
             y <- phase
             # sin(x+y) = sin(x)cos(y) + cos(x)sin(y)
             # sin(freq * f0 * t + phase)
             c(sin(x+y))
             #a0 + sum(ampli * sin(x + y))
      }) %>% t()
}
