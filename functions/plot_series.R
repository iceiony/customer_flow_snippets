plot_series <- function(series, shop_ids = unique(series$shop_id)){
    if(is.null(dim(series))) series <- t(series)

    series <- as.data.frame(series)

    if(!'shop_id' %in% colnames(series)){
        series$shop_id <- rownames(series) 
    }

    series <- filter(series, shop_id %in% shop_ids) %>%
                melt(id.vars = c('shop_id'))
    fig   <- ggplot(series, 
                    aes(x = as.numeric(variable),
                        y = value,
                        colour = as.factor(shop_id))) + 
             geom_line()

    plot(fig)
}
