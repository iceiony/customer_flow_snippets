source('./init.R')
source('./mlp_functions/init.R')

prepare <- function(x, trended){
    x <- unlist(x) %>% tail(-1)
    not_na <- first(which(!is.na(x)))
    x <- x[-seq(not_na - 1)]
    #x[is.na(x)] <- 0
    #nas <- which(is.na(x))
    #nas[nas <= 1]          <- 1 + 1
    #nas[nas >= length(x)]  <- length(x) - 1
    #x[nas + 1] <- NA
    #x[nas - 1] <- NA
    (normalise(x, trended) * 10) 
}

mlp_train <- function(sales, params){
    list2env(params, environment())
    s <- prepare(sales, trended)

    net  <- train_network(s, pre_sales, hidd, rate, train_len)
    net$train_error <- mean(tail(net$errors), 50) 
    #plot(net$errors, type = 'l', ylim = c(0, 100) , xlim=c(0, 1000))
    #net$train_error

    return(net)
}

mlp_predict <- function(sales, params, net, duration){
    list2env(params, environment())
    s <- prepare(sales, trended)

    pred <- tail(s, pre_sales) %>% predict_future(net, duration)
    pred <- (pred / 10) %>% denormalise(attributes(s), trended)
    
    return(pred)
}

predict_future <- function(sales, net, duration = 14){
    sales[is.na(sales)] <- 0

    pred <- c()
    for(sale_idx in seq(duration)){
        next_day <- run_network(sales , net$w) %>% last()
        pred     <- c(pred, next_day)
        sales    <- c(sales[-1], next_day)
    }

    tail(pred, duration)
}
