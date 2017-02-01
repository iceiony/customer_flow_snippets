plot_sales  <- function(sales, shop_ids){
    sales <- melt(shop_sales, id.vars = 'shop_id')
    fig   <- filter(sales, shop_id %in% shop_ids) %>%
                  ggplot(aes(x = as.numeric(variable),
                             y = value,
                             colour = as.factor(shop_id))) + 
                  geom_line()

    plot(fig)
}
