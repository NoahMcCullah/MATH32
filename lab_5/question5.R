rm(list=ls(all=TRUE)) #clears memory

alpha = 0
beta = 20
x = seq(from=alpha, to=beta, by=0.1)
lambda = 0.25
expo = pexp(x, rate=lambda)
penpaper = -(exp(-lambda*x))+1
plot(x, expo, type = 'l', ylim = c(0,1))
lines(x, penpaper, type = 'l')