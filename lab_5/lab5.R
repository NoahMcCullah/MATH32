rm(list=ls(all=TRUE)) #clears memory

means = runif(5, min=-5, max=5)

one1 = pnorm(means[1]+1, mean=means[1], sd=1) - pnorm(means[1]-1, mean=means[1], sd=1)
one2 = pnorm(means[2]+1, mean=means[2], sd=1) - pnorm(means[2]-1, mean=means[2], sd=1)
one3 = pnorm(means[3]+1, mean=means[3], sd=1) - pnorm(means[3]-1, mean=means[3], sd=1)
one4 = pnorm(means[4]+1, mean=means[4], sd=1) - pnorm(means[4]-1, mean=means[4], sd=1)
one5 = pnorm(means[5]+1, mean=means[5], sd=1) - pnorm(means[5]-1, mean=means[5], sd=1)
print("Within one standard deviation: ")
print(one1)
print(one2)
print(one3)
print(one4)
print(one5)

two1 = pnorm(means[1]+(2^2), mean=means[1], sd=2) - pnorm(means[1]-(2^2), mean=means[1], sd=2)
two2 = pnorm(means[2]+(2^2), mean=means[2], sd=2) - pnorm(means[2]-(2^2), mean=means[2], sd=2)
two3 = pnorm(means[3]+(2^2), mean=means[3], sd=2) - pnorm(means[3]-(2^2), mean=means[3], sd=2)
two4 = pnorm(means[4]+(2^2), mean=means[4], sd=2) - pnorm(means[4]-(2^2), mean=means[4], sd=2)
two5 = pnorm(means[5]+(2^2), mean=means[5], sd=2) - pnorm(means[5]-(2^2), mean=means[5], sd=2)
print("Within two standard deviations: ")
print(two1)
print(two2)
print(two3)
print(two4)
print(two5)

three1 = pnorm(means[1]+(3^2), mean=means[1], sd=3) - pnorm(means[1]-(3^2), mean=means[1], sd=3)
three2 = pnorm(means[2]+(3^2), mean=means[2], sd=3) - pnorm(means[2]-(3^2), mean=means[2], sd=3)
three3 = pnorm(means[3]+(3^2), mean=means[3], sd=3) - pnorm(means[3]-(3^2), mean=means[3], sd=3)
three4 = pnorm(means[4]+(3^2), mean=means[4], sd=3) - pnorm(means[4]-(3^2), mean=means[4], sd=3)
three5 = pnorm(means[5]+(3^2), mean=means[5], sd=3) - pnorm(means[5]-(3^2), mean=means[5], sd=3)
print("Within three standard deviations: ")
print(three1)
print(three2)
print(three3)
print(three4)
print(three5)
####################################
#####################################
####################################
###PART 2###
m = 0.60
s = sqrt(0.0175)
#
#a
#
x = seq(from=0, to=1, by=0.01)
pdf = dnorm(x, mean = m, sd = s)
#
#b
# P(neg) = 0
# P(X > 1) = 0
gradeNeg = pnorm(0, mean=m, sd=s)
print(gradeNeg)
gradeExtra = 1-pnorm(1, mean=m, sd=s)
print(gradeExtra)

#
#c
#
z = pnorm(0.9, mean = m, sd = s)
message(sprintf("90th percentile: %f", z))

#
#d
#
message(sprintf("Probability student get an A: %f%%", 100*(1-z)))

#
#e
#
passing = pnorm(0.5, mean=m, sd=s)
message(sprintf("Probability of a passing grade: %f%%", 100*(1-passing)))
###############################################
################################################
#################################################
###PART 3###
alpha = -10
beta = 5
domain = seq(from=alpha, to=beta, by=0.01)
#f(a) = 1/15x + 2/3
paper = (domain/15)+(2/3)
cdf = punif(domain, alpha, beta)
plot(domain, cdf, type = 'l', ylim = c(0,1))
lines(domain, paper, type ='l')
#################################################
#################################################
#################################################
###PART 4###
alpha = -10
beta = 5

quant = qunif(c(0,1), alpha, beta)
pen = (15*c(0,1))+(-10)
plot(c(0,1), quant, type = 'l')
lines(c(0,1), pen, type = 'l')
###############################################
#################################################
################################################3333
###PART 5###
alpha = 0
beta = 20
x = seq(from=alpha, to=beta, by=0.1)
lambda = 0.25
expo = pexp(x, rate=lambda)
penpaper = -(exp(-lambda*x))+1
plot(x, expo, type = 'l', ylim = c(0,1))
lines(x, penpaper, type = 'l')
##############################################
##############################################
##############################################
###PART 6###
lambda = 0.25
alpha = 0
beta = 1
x = seq(from=alpha, to=beta, by=0.1)

que6 = qexp(x, rate=lambda)
equation = -(exp(-lambda*x))+1
plot(x, que6, type = 'l', ylim=c(0,10))
lines(x, equation, type = 'l')