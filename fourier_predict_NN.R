source('./init.R')
source('./mlp_functions/init.R')
source('./fourier_functions/init.R')
library('smoother')

shop_sales  <- read.table('./data/daily_shop_sales.csv', sep = ',', header = T)

shop   <- prepare(shop_sales, 11, 7 * 8)
signal <- shop %>% smth.gaussian(0.1, tails = T) %>% head(-14)
trend  <- linear_trend(signal)
plot(signal,,'l');abline(trend)
signal <- signal %>% trend$remove() %>% normalise()

net  <- learn(signal)
pred <- predict(net, length(signal) + 14 )%>%
        #smth.gaussian(window = 0.1, tails = T) %>%
        identity()

attributes(pred) <- attributes(signal)
signal <- denormalise(signal) %>% trend$add()
pred   <- denormalise(pred)   %>% trend$add()

plot(shop,,'l',col='red',lwd=3); 
lines(signal,,lwd=2,col='blue');
lines(pred,,lwd=2);
report_error(signal, shop, 14)
report_error(pred, shop, 14)
cor(pred,shop)

#make the overall prediction into a trend 
trend  <- pred
signal <- shop %>% head(-14) 
signal <- (signal - head(trend,-14)) %>% normalise()

net  <- learn(signal)
pred <- predict(net, length(signal) + 14)

plot(pred,,'l',lwd=2, col='red')
lines(signal,)
cor(signal,head(pred,-14))

attributes(pred) <- attributes(signal)[c('min', 'norm')]
pred   <- denormalise(pred) + trend


plot(shop,,'l',col='red',lwd=3); 
#lines(signal,,lwd=2,col='blue');
lines(pred,,lwd=2);
report_error(pred, shop, 14)
cor(pred,shop)

