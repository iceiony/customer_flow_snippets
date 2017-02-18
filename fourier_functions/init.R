
# source all functions from mlp_functions folder
sources <- list.files('./fourier_functions','[.]R$',full.names=T) 
sources <- Filter(f(!grepl('init.R',x)), sources) 

invisible(lapply(sources, source))


