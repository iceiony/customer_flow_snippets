source('./init.R')
source('./mlp_functions/init.R')
source('./fourier_functions/init.R')

shop_sales  <- read.table('./data/daily_shop_sales.csv', sep = ',', header = T)

shop   <- prepare(shop_sales[3,-1])
signal <- prepare(shop_sales[3,-1], 0.2)
trend  <- linear_trend(signal, -14)
signal <- signal %>% trend$remove() %>% normalise()

net  <- learn(signal, -14)
pred <- predict(net, length(signal))  %>% smth.gaussian(window = 0.3, tails = T) 

attributes(pred) <- attributes(signal)
signal <- denormalise(signal) %>% trend$add()
pred   <- denormalise(pred)   %>% trend$add()

plot(shop,,'l',col='red',lwd=3); lines(signal,,lwd=2,col='blue'); lines(pred,,lwd=2);
report_error(pred, shop, 14)
