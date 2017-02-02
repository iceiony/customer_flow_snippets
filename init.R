library('pryr')
library('plyr')
library('dplyr')
library('reshape2')
library('ggplot2')
library('parallel')
library('tictoc')

# source all functions from mlp_functions folder
sources <- lapply(list.files('functions', '[.]R$', full.names = T), source)

elapsed <- function(msg){
    toc()
    message(msg)
    tic()
}
