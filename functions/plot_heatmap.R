plot_heatmap <- function(dat){
    heatmap(dat, main = 'shop sales',  
        Rowv = NA, Colv = NA,
        ylab = 'shops', xlab = 'days',
        labRow = NA, labCol = NA)
}
