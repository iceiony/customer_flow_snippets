init_weights <- function(hidden_layers){
   w <- hidden_layers %>% 
        rollapplyr(2, f(x + c(1,0))) %>%
        alply(1, function(x){
	         array(rnorm(prod(x), sd = 0.5), dim = x)
	   })

   return(w)
}
