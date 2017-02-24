make_prediction <- function(signal, trended = FALSE){
    trend <- if(!trended) linear_trend(signal) else null_trend()

    signal <- signal %>% trend$remove() %>% normalise()

    net  <- learn(signal, 0)
    pred <- predict(net, length(signal) + 14)

    attributes(pred) <- attributes(signal)[c('min', 'norm')]
    pred   <- denormalise(pred) %>% trend$add()
    #signal <- denormalise(signal) %>% trend$add()

    pred
}
