single_layer_train <- function(IN,TARG){
#trains a single layer regression network using matrix inversion
    require('MASS')
    IN <- cbind(1, IN) #add bias unit
    net <- list(w = list(ginv(IN) %*% TARG))
    net
}
