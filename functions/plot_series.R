plot_series <- function(series, shop_ids = unique(series$shop_id)){
    series <- as.data.frame(series)
    series <- filter(series, shop_id %in% shop_ids) %>%
                melt(id.vars = c('shop_id'))
    fig   <- ggplot(series, 
                    aes(x = as.numeric(variable),
                        y = value,
                        colour = as.factor(shop_id))) + 
             geom_line()

    plot(fig)
}
