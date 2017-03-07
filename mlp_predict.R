source('./init.R')
source('./mlp_functions/init.R')

prepare <- function(x){
    x <- unlist(x) %>% tail(-1)
    not_na <- first(which(!is.na(x)))
    x <- x[-seq(not_na - 1)]
    x[is.na(x)] <- 0
    (normalise(x) * 10)
}

predict_future <- function(IN, net, total_period, duration = 14){
    pred <- c()
    for(i in seq(duration)){
        moment   <- 5 * (total_period + i) / total_period
        next_day <- run_network(c(IN, moment), net$w) %>% last()
        pred <- c(pred, next_day)
        IN <- c(IN[-1], next_day)
    }
    tail(pred, duration)
}

shop_sales <- read.table('./data/daily_shop_sales.csv', sep = ',', header = T)

message('Preparing training data...')
s <- shop_sales[1, -1] %>% prepare()
train <- head(s, -14)

period <- 14
total_period <- length(train) - period - 1
train <- lapply(seq(total_period), 
         function(idx){
             segment <- seq(idx, length.out = period)
             moment  <- 5 * idx / total_period  
             IN  <- train[segment]
             OUT <- train[idx + period + 1]
             c(OUT, IN, moment) 
      }) 
train <- do.call(rbind, train)

IN  <- train[, -1, drop = F]
OUT <- train[,  1, drop = F]

message('Training network...')
hidden <- c(24)
net <- train_network(IN, OUT, hidden, rep(5e-5, 2), 300)
plot(net$errors, type = 'l', ylim = c(0, max(net$errors, na.rm = T)), xlim=c(0, 500))
cat('Train error:'  , tail(net$errors,1), '\n')

out <- run_network(IN,net$w) %>% last()
dat <- cbind(OUT,out) %>% t()
rownames(dat) <- c('actual','learned')
plot_series(dat)

message('Predict future days')
pred <- head(s,-14) %>% tail(period) %>% predict_future(net, total_period, 14)
valid <- tail(s, 14)
plot_series(rbind(pred,valid))

attributes(pred) <- attributes(s)[c('min','norm')]
attributes(valid) <- attributes(s)[c('min','norm')]
pred  <- (pred  / 10) %>% denormalise()
valid <- (valid / 10) %>% denormalise()

report_error(pred,valid)

