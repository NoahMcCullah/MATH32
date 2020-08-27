sizevec = 2 ^ (c(5:16))

ns = length(sizevec)

heads = numeric(length = ns)
results = numeric(length = ns)

for(i in c(1:ns))
{
  flips = sample(x=3, size=sizevec[i], replace=TRUE)
  heads[i] = sum(flips==1)
  results[i] = sum(heads[1:i])/sum(sizevec[1:i])
}

plot(c(5:16), results, type='b', xlab='log_2(num flips)')
abline(h=0.33, col='red')