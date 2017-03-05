source('init.R')
source('./mlp_functions/init.R')
source('./fourier_functions/init.R')
library('smoother')
library('fastICA')
library('forecast')
library('MASS')

shop_views[is.na(shop_views)] <- 0
shop_sales[is.na(shop_sales)] <- 0

cls = 2

group  <- (filter(shop_sales, cluster == cls)) %>% 
            apply(1,function(x) {
               x[-seq(2)] %>% smth.gaussian(0.02, tails=T) %>% head(-14)
            }) %>% t() 
group <- group[,-seq(150)]

group2  <- (filter(shop_sales, cluster == cls)) %>% 
            apply(1,function(x) {
               x[-seq(2)] %>% smth.gaussian(0.02, tails=T)# %>% head(-14)
            }) %>% t() 
group2 <- group2[,-seq(150)]

a <- fastICA(t(group), 70)
S <- apply(a$S, 2, function(s){
                fit <- auto.arima(s, seasonal = T) 
                fc  <- forecast(fit, 14)
                c(s , fc$mean)
          })

#X <- a$S %*% a$A %>% t() 
X <- S %*% a$A %>% t()
X <- apply(X, 1, function(x) {
               x <- x - min(x)
          }) %>% t()

#dev.new();plot_series(X)

score <- sapply(seq(nrow(X)),
            function(idx){
                x <- X[idx,] %>% tail(14)
                g <- group2[idx,] %>% tail(14)
                #plot_series(rbind(x,g))
                stats_err(x,g)$err
         })
score
score %>% mean()
