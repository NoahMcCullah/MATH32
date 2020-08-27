rm(list=ls(all=TRUE)) #clears memory

alpha = -10
beta = 5

quant = qunif(c(0,1), alpha, beta)
pen = (15*c(0,1))+(-10)
plot(c(0,1), quant, type = 'l')
lines(c(0,1), pen, type = 'l')