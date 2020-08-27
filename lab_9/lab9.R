rm(list = ls(all=TRUE))

source('lab9pareto.R')

##############################################
#                Task 1
range = seq(from=1,to=5,by=0.00001)

xVal = rpareto(range, 3)
yVal = rpareto(range, 3)

zVal = 0.5*(xVal + yVal)
cdf = ppareto(zVal, shape = 3)

#plot(cdf)

print(mean(zVal))

print(var(zVal))
##############################################
###############################################
#               Task 2
#constructing mxn matrix
M = 1000
N = 10000
x = matrix(0, M, N)

#fill matrix with Pareto(3) samples
for(i in c(1:N)){
  x[,i] = rpareto(M, 3)
}

xbar = colMeans(x)
plot(ecdf(xbar))

#the mean and var components of the plot below
print(mean(colMeans(x)))
print(var(colMeans(x)))

xvec = seq(from = min(xbar), to = max(xbar), by = 0.1)
points(xvec, pnorm(xvec, mean = mean(colMeans(x)), sd = var(colMeans(x))), pch = 20, col = "red")

#############################################################################
############################################################################
#             Task 3
# As n=10 and m increases, the plot does not converge. The variance decreases by about a
# power of 10 and the mean stablizes around 1.49. We are increasing the precision of the
# points, not the number of points

# As m = 1000 and n increases, the labels on the x-axis increase in the positive x direction.
# If n were to increase to positive infinity, the plot would continue to stretch in the
# pos x direction, representing a continuous normal distribution

