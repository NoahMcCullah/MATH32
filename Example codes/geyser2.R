rm(list=ls(all=TRUE))

load('geyser.RData')

# import pause() function
library('DAAG')

# this function implements the Epanechnikov kernel
# see the formula for Epanechnikov kernel K(u) on page 213

# the BUMP
myepa <- function(u)
{
	n = length(u)
	k = 0*c(1:n)
	dom = which(abs(u)<=1)
	k[dom] = 0.75*(1 - u[dom]^2)
	return(k)
}

# TRIANGULAR BUMP
mytri <- function(u)
{
	n = length(u)
	k = numeric(length=n)
	dom = which(abs(u)<=1)
	k[dom] = 1 - abs(u[dom])
	return(k)
}

# compute and plot kernel density estimate (KDE)
# this is done "by hand" for pedagogical purposes only

n = length(g)

# bandwidth parameter h
h = 23.65
h2 = h/6
# h = 2
# h = 150

# vector of points tvec at which we'd like to plot the KDE
npts = 1000
r0 = min(g)*0.5
r1 = max(g)*1.5
tvec = seq(from=r0,to=r1,length.out=npts)  # x in the notes

# initialize KDE to be a vector of 0's
rawkde = 0*c(1:npts)
rawkde2 = 0*c(1:npts)
mykde = 0*c(1:npts)

# add one data point at a time
for (j in seq(from=1,to=272,by=1))
{
  dropfun0 = dnorm((tvec - g[j])/h2)/h2
	mykde = mykde + dropfun0
	
	dropfun1 = myepa((tvec - g[j])/h)/h
	dropfun2 = mytri((tvec - g[j])/h)/h
	rawkde = rawkde + dropfun1
	rawkde2 = rawkde2 + dropfun2
	# normalized version
  # if you want to plot the KDE as it gets built, one data pt at a time
	#pause()
}
plot(tvec,rawkde2/j,xlab="x",ylab="probability density",type='l',ylim=c(0,0.015))
lines(tvec,rawkde/j,xlab="x",ylab="probability density",col='red')
lines(tvec,mykde/j,xlab="x",ylab="probability density",col='blue')


# if you only care about final KDE
# kde = rawkde/n
#kde2 = rawkde2/n
#plot(tvec,kde2,col='black',xlab="x",ylab="probability density")
# lines(t,kde2,col='blue')


