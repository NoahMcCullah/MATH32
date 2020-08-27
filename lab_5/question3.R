rm(list=ls(all=TRUE)) #clears memory

alpha = -10
beta = 5
domain = seq(from=alpha, to=beta, by=0.01)
#f(a) = 1/15x + 2/3
paper = (domain/15)+(2/3)
cdf = punif(domain, alpha, beta)
plot(domain, cdf, type = 'l',color = 'blue', ylim = c(0,1))
lines(domain, paper, type ='l', color = 'red')