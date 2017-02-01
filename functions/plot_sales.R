plot_sales  <- function(sales, shop_ids){
    sales <- filter(sales, shop_id %in% shop_ids) %>%
             melt(id.vars = 'shop_id')
    fig   <- ggplot(sales, 
                    aes(x = as.numeric(variable),
                        y = value,
                        colour = as.factor(shop_id))) + 
             geom_line()

    plot(fig)
}
