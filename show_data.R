source('init.R')

shop_views <- read.table('./data/daily_shop_view.csv' , sep = ',', header = T)
shop_sales <- read.table('./data/daily_shop_sales.csv', sep = ',', header = T)

found <- shop_sales$shop_id %in% shop_views$shop_id
shop_sales <- shop_sales[found,]

sort_order <- bind_rows( apply(shop_sales[,-1], 1, 
                function(x) {
                    data.frame(
                        one = first(which(!is.na(x))),
                        two = sum(!is.na(x))
                    )
                }))
sort_order <- order(sort_order$one, -sort_order$two, decreasing = T) 
shop_sales <- shop_sales[sort_order,]
shop_views <- shop_views[sort_order,]

sales <- as.matrix2(shop_sales, process = f(log(x)^3) )
png('figures/shop_sales_heat.png', width = 2000, height = 2000)
plot_heatmap(sales[,-1])
dev.off()

views <- as.matrix2(shop_views, ncol(shop_sales), f(log(x)^(1.2)))
png('figures/shop_views_heat.png', width = 2000, height = 2000)
plot_heatmap(views[,-1])
dev.off()

x <- sales[1997,,drop=F]
y <- views[1997,,drop=F]
nms <- colnames(x)
x[is.na(x)] <- median(x,na.rm = T) 
y[is.na(y)] <- median(y,na.rm = T)
x <- lowess(seq(length(x)), x, f=1/8)$y
y <- lowess(seq(length(y)), y, f=1/8)$y
x <- (x / mean(x, na.rm = T)) * 8
y <- (y / mean(y, na.rm = T)) * 8.5

dat <- rbind(x,y)
colnames(dat) <- nms
plot_series(dat)

png('test.png', width = 2000, height = 2000)
plot_heatmap(dat[,-1])
dev.off()

