plot_heatmap <- function(dat){
    require('RColorBrewer')
    rf <- colorRampPalette(rev(brewer.pal(8, 'Accent')))
    heatmap(dat, main = 'shop sales',  
        Rowv = NA, Colv = NA,
        ylab = 'shops', xlab = 'days',
        labRow = NA, labCol = NA,
        col = rf(5000))
}
