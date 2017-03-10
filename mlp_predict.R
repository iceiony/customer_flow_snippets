source('./init.R')
source('./mlp_functions/init.R')
library('smoother')

shop_id <- 3
period  <- 20
vperiod <- 20

trend <- function(s, gaus){
    trend <- unlist(s) %>% smth.gaussian(gaus, tails = T)
    trend# * 0.9
}

prepare <- function(x, chop_start = Inf, chop_end = 0){
    x <- unlist(x) %>% tail(-1) %>% tail(chop_start + 1)

    if(chop_end > 0) x <- tail(x, -chop_end)

    not_na <- first(which(!is.na(x)))
    x <- x[-seq(not_na - 1)]
    #x[is.na(x)] <- 0
    #nas <- which(is.na(x))
    #nas[nas <= 1]          <- 1 + 1
    #nas[nas >= length(x)]  <- length(x) - 1
    #x[nas + 1] <- NA
    #x[nas - 1] <- NA
    (normalise(x) * 10)
}

shop_sales <- read.table('./data/daily_shop_sales.csv', sep = ',', header = T)
shop_views <- read.table('./data/daily_shop_view.csv', sep = ',', header = T)

message('Preparing training data...')
chop <- sample(21, 1) - 1
v <- shop_views[shop_id, -1] %>% prepare(chop_end = -chop)
s <- shop_sales[shop_id, -1] %>% prepare(length(v), chop_end = -chop)

q <- mean(v, na.rm = T)#trend(v, 0.25)
r <- mean(s, na.rm = T)#trend(s, 0.25)
#plot_series(rbind(s,r))
v <- (v - q)
s <- (s - r)
#plot_series(s)

train <- head(s, -14)
view  <- head(v, -14)

message('Training network...')
hidden <- c(300, 90)
net <- train_network(train, view, hidden, c(5e-1, 1e-3, 1e-2), round((length(s) / 100) * 300) ) 
#plot(net$errors, type = 'l', ylim = c(0, 100) , xlim=c(0, 1000))
cat('Train error:'  , mean(tail(net$errors), 50), '\n')

#dat <- cbind(OUT,out) %>% t()
#rownames(dat) <- c('actual','learned')
#plot_series(dat)

message('Predict future days')
predict_future <- function(IN, VIEW, net, duration = 14){
    VIEW[is.na(VIEW)] <- 0
    IN[is.na(IN)]     <- 0
    pred <- c()
    for(i in seq(duration)){
        v <- VIEW[seq(vperiod) + i - 1]
        next_day <- run_network(c(IN, v) , net$w) %>% last()
        pred     <- c(pred, next_day)
        IN       <- c(IN[-1], next_day)
    }
    tail(pred, duration)
}

inn  <- head(s, -14) %>% tail(period)
view <- head(v, -14) %>% tail(period + vperiod)
pred <- predict_future(inn, view, net, 14)
pred <- pred + tail(r, 14)
valid <- tail(s + r, 14)
#plot_series(rbind(pred,valid))

attributes(valid) <- attributes(pred) <- attributes(s)[c('min','norm')]
pred  <- (pred  / 10) %>% denormalise()
valid <- (valid / 10) %>% denormalise()

report_error(pred,valid)
