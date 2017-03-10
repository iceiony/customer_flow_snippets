trend <- function(s, gaus){
    require('smoother')
    trend <- unlist(s) %>% smth.gaussian(gaus, tails = T)
    trend# * 0.9
}
