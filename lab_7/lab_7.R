#collaborated with Nick Carreon
# clear memory
rm(list=ls(all=TRUE))

# simulate data
set.seed(32)
npts = 32
x = seq(from=-1,to=1,length.out=npts)
y = x + rnorm(n=npts,mean=0,sd=0.5)

# split into test and train
trainind = seq(from=1,to=32,by=2)
xtrain = x[trainind]
ytrain = y[trainind]
xtest = x[-trainind]
ytest = y[-trainind]

# plot training set data


# build model using training set
numpow = 16

train = data.frame(x=xtrain,y=ytrain)
test = data.frame(x=xtest,y=ytest)

#function calculating mean squared error
mse = function(data,y,mylm){
  meanse = mean((y - predict(mylm, newdata = data))^2)
  return(meanse)
}

#create empty vectors of size 16
plot.data.x= c(1:numpow)*0
plot.data.y.train = c(1:numpow)*0
plot.data.y.test = c(1:numpow)*0

#loop through values of numpow
for (i in 1:numpow) {
  xnam = paste("I(x**",c(1:i),")",sep='')
  regression = as.formula(paste("y ~ ", paste(xnam, collapse="+")))
  
  mylm = lm(regression, data=train)
  plot.data.x[i]=i
  #create plots for test and training data
  plot.data.y.train[i] = mse(train,train$y,mylm)
  plot.data.y.test[i] = mse(test,test$y,mylm)
}
#plots showing overfitting mse is massively inaccurate at large intervals
par(mfrow = c(2,1))
plot(plot.data.x, plot.data.y.train, pch = 19,col ='red',xlab='x',ylab='y',ylim= c(0,0.5))

par(new=T)
plot(plot.data.x, plot.data.y.test, pch =18,col ='blue',xlab='x',ylab='y',ylim= c(0,0.5))
plot(plot.data.x, plot.data.y.train, pch =19,col ='red',xlab='x',ylab='y',ylim= c(0,50000))

par(new=T)
plot(plot.data.x, plot.data.y.test, pch =18,col ='blue',xlab='x',ylab='y',ylim= c(0,50000))

