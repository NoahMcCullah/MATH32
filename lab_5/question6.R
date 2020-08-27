rm(list=ls(all=TRUE)) #clears memory

lambda = 0.25
alpha = 0
beta = 1
x = seq(from=alpha, to=beta, by=0.1)

que6 = qexp(x, rate=lambda)
equation = -(exp(-lambda*x))+1
plot(x, que6, type = 'l', ylim=c(0,10))
lines(x, equation, type = 'l')