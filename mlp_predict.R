source('./init.R')
source('./mlp_functions/init.R')

prepare <- function(x, max_len = Inf){
    x <- unlist(x) %>% tail(-1)
    not_na <- first(which(!is.na(x)))
    x <- x[-seq(not_na - 1)]
    #x[is.na(x)] <- 0
    #nas <- which(is.na(x))
    #nas[nas <= 1]          <- 1 + 1
    #nas[nas >= length(x)]  <- length(x) - 1
    #x[nas + 1] <- NA
    #x[nas - 1] <- NA
    x <- tail(x, max_len )
    (normalise(x) * 10) 
}

mlp_train <- function(views, sales, pre_views, pre_sales){
    v <- prepare(views)
    s <- prepare(sales, length(v))

    hidd <- c(150, 50)
    rate <- c(5e-2, 1e-2, 1e-2) 
    len  <- round((length(s) / 100) * 15)  
    net  <- train_network(v, s, pre_views, pre_sales, hidd, rate, len)
    net$train_error <- mean(tail(net$errors), 50) 
    #plot(net$errors, type = 'l', ylim = c(0, 100) , xlim=c(0, 1000))
    #net$train_error

    return(net)
}

mlp_predict <- function(views, sales, pre_views, pre_sales, net, duration){
    v <- prepare(views)
    s <- prepare(sales, length(v))

    pred <- predict_future(tail(v, pre_views), tail(s, pre_sales), net, duration)
    pred <- (pred / 10) %>% denormalise(attributes(s))
    
    return(pred)
}

predict_future <- function(views, sales, net, duration = 14){
    views[is.na(views)] <- 0
    sales[is.na(sales)] <- 0

    pred <- c()
    view_idx  <- seq(length(views) - length(sales)) - 1
    all_views <- views
    for(sale_idx in seq(duration)){
        views    <- all_views[view_idx + sale_idx]
        next_day <- run_network(c(views, sales) , net$w) %>% last()
        pred     <- c(pred, next_day)
        sales    <- c(sales[-1], next_day)
    }
    tail(pred, duration)
}
