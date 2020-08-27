#clear memory
rm(list=ls(all=TRUE))

#change directory
#setwd('../lec8')

#data import
auto = read.csv('Auto.csv', stringsAsFactors = FALSE)
#dim(auto)

#summary of all columns
#print(summary(auto))

#histogram of mpgs
#hist(auto$mpg, break = 10)
#normalized histogram, boxes are resized to add to 1
#hist(auto$mpg, breaks=c(9:47), probability = TRUE)

#boxplot
#boxplot(auto$weight)

#BE CAREFUL WITH THIS
#can make scatterplots with all columns, but only with numeric values
#save a thing to a file
#pdf('scattermatrix.pdf')
#pairs(auto[,-9])
#dev.off()

#Linear Regression
#linear model
#mylm = lm(mpg ~ weight, data = auto)
#print(summary(mylm))
#abline(mylm)

#mean squared error
#mean(auto$mpg - predict(mylm)^2)

#trainrows = sample(397, 317, replace = FALSE)
#autotrain = auto[trainrows, ]
#autotest = auto[-trainrows, ]
#plot(autotrain$weight, autotrain$mpg)
#mylm = lm(mpg ~ weight, data = autotrain)
#print(mean(abs(autotest$mpg - predict(mylm, newdata = autotest))))

#create a quadratic model
#mpg = beta0 + beta1 *weight + beta2 * (weight^2)
#mylm2 = lm(mpg ~ weight + I(weight**2), data = autotrain)

#plot curve
#weightvec = seq(from = min(auto$weight), to = max(auto$weight), length.)
#betas = coef(mylm2)
#quadcurve = betas[1] + betas[2] * weightvec + betas[3] * (weight ^ 2)
#lines(weightvec, quadcurve, col = 'red', lwd = 2)

