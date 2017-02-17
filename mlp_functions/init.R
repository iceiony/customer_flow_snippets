library('pryr')
library('plyr')
library('dplyr')
library('purrr')
library('sigmoid')
library('zoo')

# source all functions from mlp_functions folder
sources <- list.files('./mlp_functions','[.]R$',full.names=T) 
sources <- Filter(f(!grepl('init.R',x)), sources) 

invisible(lapply(sources, source))


