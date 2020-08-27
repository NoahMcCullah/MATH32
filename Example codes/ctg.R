# clear memory
rm(list=ls(all=TRUE))

# load CTG data
load('ctg.RData')

# make training and test sets
trainfrac = 0.80
nrows = ceiling(0.8*nrow(m))

# randomly choose "nrows" number of rows for the training set
trainrows = sample(c(1:nrow(m)), nrows)

# form the training sets from m and q
mtrain = m[trainrows, ]
qtrain = q[trainrows]

# form the test sets (everything not in training sets)
mtest = m[-trainrows, ]
qtest = q[-trainrows]

mythresh <- function(q)
{
  q[which(q>=0.5)] = 1
  q[which(q<0.5)] = 0
  return(q)
}

# confusion matrix function
myconfusion = function(pfit, y)
{
  confusion = matrix(nrow=2,ncol=2)
  actual0 = which(y==0)
  actual1 = which(y==1)
  confusion[1,1] = sum(pfit[actual1]==1)
  confusion[2,1] = sum(pfit[actual0]==1)
  confusion[1,2] = sum(pfit[actual1]==0)
  confusion[2,2] = sum(pfit[actual0]==0)
  return(confusion)
}

# let's try to predict whether a fetus is normal or abnormal
# using logistic regression

# Model #1: Use All The Variables!
print("__________ Model 1 __________")
model1 = glm(qtrain ~ ., data=mtrain, family=binomial(link='logit'))

# print training model summary
pfit = mythresh(predict(model1, type='response'))
conf = myconfusion(pfit, qtrain)
n = length(qtrain)
print(100*(conf[1,1]+conf[2,2])/n)

# print test model summary
pfit = mythresh(predict(model1, newdata = mtest, type='response'))
conf = myconfusion(pfit, qtest)
n = length(qtest)
print(100*(conf[1,1]+conf[2,2])/n)

print(conf)

# Model 2: use only the *** variables
print("__________ Model 2 __________")
model2 = glm(qtrain ~ AC + UC + DP + ASTV + ALTV + Width + Min + Variance,
             data=mtrain, family=binomial(link='logit'))

# print training model summary
pfit = mythresh(predict(model2, type='response'))
conf = myconfusion(pfit, qtrain)
n = length(qtrain)
print(100*(conf[1,1]+conf[2,2])/n)

# print test model summary
pfit = mythresh(predict(model2, newdata = mtest, type='response'))
conf = myconfusion(pfit, qtest)
n = length(qtest)
print(100*(conf[1,1]+conf[2,2])/n)

print(conf)

# traded off some accuracy (about 1%) for interpretability


# Model 3: use only the *** variables

# balance training set
# method A: delete some 0's

# figure out number of 1's
numones = sum(qtrain==1)

# sample numones 0's
qtrainzerorows = which(qtrain==0)

# sample numones of the qtrainzerorows rows
qtrainbalzerorows = sample(qtrainzerorows, numones)
qtrainbal = c(qtrain[which(qtrain==1)], qtrain[qtrainbalzerorows])
mtrainbal = rbind(mtrain[which(qtrain==1),], mtrain[qtrainbalzerorows,])

print("__________ Model 3A __________")
model3a = glm(qtrainbal ~ AC + UC + DP + ASTV + ALTV + Width + Min + Variance,
              data=mtrainbal, family=binomial(link='logit'))

# print training model summary
pfit = mythresh(predict(model3a, type='response'))
conf = myconfusion(pfit, qtrainbal)
n = length(qtrainbal)
print(100*(conf[1,1]+conf[2,2])/n)

# print test model summary
pfit = mythresh(predict(model3a, newdata = mtest, type='response'))
conf = myconfusion(pfit, qtest)
n = length(qtest)
print(100*(conf[1,1]+conf[2,2])/n)

print(conf)

# balance training set
# method B: amplify the number of 1's

# figure out number of 0's
numzeros = sum(qtrain==0)

# sample numones of the qtrainonerows rows
qtrainonerows = which(qtrain==1)
qtrainbalonerows = sample(qtrainonerows, numzeros, replace=TRUE)
qtrainbal = c(qtrain[which(qtrain==0)], qtrain[qtrainbalonerows])
mtrainbal = rbind(mtrain[which(qtrain==0),], mtrain[qtrainbalonerows,])

print("__________ Model 3B __________")
model3b = glm(qtrainbal ~ I(UC*ALTV) + AC + UC + DP + ASTV + ALTV + Width + Min + Variance,
              data=mtrainbal, family=binomial(link='logit'))

# print training model summary
pfit = mythresh(predict(model3b, type='response'))
conf = myconfusion(pfit, qtrainbal)
n = length(qtrainbal)
print(100*(conf[1,1]+conf[2,2])/n)

# print test model summary
pfit = mythresh(predict(model3b, newdata = mtest, type='response'))
conf = myconfusion(pfit, qtest)
n = length(qtest)
print(100*(conf[1,1]+conf[2,2])/n)

print(conf)

# IDEAS:
# add back some of the ** and * variables
# add products of variables


