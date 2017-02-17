update_weights <- function(w,delta,rate){
    Map(f(w,d,r, w + r * d ), w, delta, rate)
}
