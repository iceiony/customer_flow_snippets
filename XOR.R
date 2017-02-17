rm(list=ls())
source('./init.R')
source('./mlp_functions/init.R')

IN   <- c(1, 0, 1, 0, 
          0, 1, 1, 0)
TARG <- c(1, 1, 0, 0)

dim(IN)   <- c(4, 2)
dim(TARG) <- c(4, 1)

hidden <- c(2)
net <- train_network(IN, TARG, hidden, c(1.5,.7), 500)

plot(net$errors, type = 'l', ylim = c(0, max(net$errors)), xlim=c(0,500))
message('Last error : ' , last(net$errors))
