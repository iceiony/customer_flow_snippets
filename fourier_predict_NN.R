source('./init.R')
source('./mlp_functions/init.R')
source('./fourier_functions/init.R')

shop_sales  <- read.table('./data/daily_shop_sales.csv', sep = ',', header = T)

shop   <- prepare(shop_sales[93,-1])
signal <- prepare(shop_sales[93,-1], 0.1)
trend  <- linear_trend(signal, -14)
signal <- signal %>% trend$remove() %>% normalise()

net  <- learn(signal[-seq(0)], -14)
pred <- predict(net, length(signal))%>%
        #smth.gaussian(window = 0.1, tails = T) %>%
        identity()

attributes(pred) <- attributes(signal)
signal <- denormalise(signal) %>% trend$add()
pred   <- denormalise(pred)   %>% trend$add()
trendf <- c(head(signal,-14), tail(pred,14))

plot(shop,,'l',col='red',lwd=3); 
lines(signal,,lwd=2,col='blue');
lines(pred,,lwd=2);
lines(trendf,,lwd=2,col='green');
report_error(signal, shop, 14)
report_error(pred, shop, 14)

#make the overall prediction into a trend 
signal <- (shop - trendf) %>% normalise()

net  <- learn(signal, -14)
pred <- predict(net, length(signal))

attributes(pred) <- attributes(signal)
signal <- denormalise(signal) + trendf
pred   <- denormalise(pred) + trendf


plot(shop,,'l',col='red',lwd=3); 
#lines(signal,,lwd=2,col='blue');
lines(pred,,lwd=2);
report_error(pred, shop, 14)

