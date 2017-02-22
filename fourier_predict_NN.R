source('./init.R')
source('./mlp_functions/init.R')
source('./fourier_functions/init.R')

shop_sales  <- read.table('./data/daily_shop_sales.csv', sep = ',', header = T)

signal <- prepare(shop_sales[1,-1], 0.2)
trend  <- linear_trend(signal)
signal <- signal %>% trend$remove() %>% normalise()

net  <- learn(signal, -14)
pred <- predict(net, length(signal))  %>% smth.gaussian(window=0.3, tails = T) 

attributes(pred) <- attributes(signal)
signal <- denormalise(signal) %>% trend$add()
pred   <- denormalise(pred)   %>% trend$add()

plot(signal,,'l',col='red',lwd=3);lines(pred,)

len_pred <- 14
pred   <- tail(pred, len_pred)
signal <- tail(signal, len_pred)
pred_err <- error_cost(pred, signal)
message('Prediction error : ', pred_err)
message('Correlation :', cor(pred, signal))

