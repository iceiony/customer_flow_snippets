source('./init.R')
source('./mlp_functions/init.R')
library('smoother')

trend <- function(s, gaus){
    trend <- unlist(s) %>% smth.gaussian(gaus, tails = T)
    trend# * 0.9
}

prepare <- function(x, chop = Inf){
    x <- unlist(x) %>% tail(-1) %>% tail(chop)
    not_na <- first(which(!is.na(x)))
    x <- x[-seq(not_na - 1)]
    x[is.na(x)] <- 0
    (normalise(x) * 10)
}

shop_sales <- read.table('./data/daily_shop_sales.csv', sep = ',', header = T)
shop_views <- read.table('./data/daily_shop_view.csv', sep = ',', header = T)

message('Preparing training data...')
v <- shop_views[3, -1] %>% prepare()
s <- shop_sales[3, -1] %>% prepare(length(v))
r <- trend(s, 0.25)
q <- trend(v, 0.25)
#plot_series(rbind(v,q))
s <- (s - r)
v <- (v - q) / 4
#plot_series(s)

train <- head(s, -14)

period  <- 20
vperiod <- 20
total_period <- length(train) - period - vperiod - 1
train <- lapply(seq(total_period) + vperiod, 
         function(idx){
             IN   <- train[seq(idx, length.out = period)]
             VIEW <- v[seq(idx - vperiod, length.out = vperiod)]
             OUT  <- train[idx + period + 1]
             c(OUT, IN, VIEW) 
      }) 
train <- do.call(rbind, train)

IN  <- train[, -1, drop = F]
OUT <- train[,  1, drop = F]

message('Training network...')
hidden <- c(96, 48)
net <- train_network(IN, OUT, hidden, c(5e-1, 5e-2, 1e-4), round((length(s) / 100) * 5) ) 
#plot(net$errors, type = 'l', ylim = c(0, max(net$errors, na.rm = T)), xlim=c(0, 300))

out <- run_network(IN,net$w) %>% last()
cat('Train error:'  , sse_cost(out,OUT), '\n')

dat <- cbind(OUT,out) %>% t()
rownames(dat) <- c('actual','learned')
#plot_series(dat)

message('Predict future days')
predict_future <- function(IN, VIEW, net, duration = 14){
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
