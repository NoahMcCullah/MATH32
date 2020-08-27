montystay = function(i){
  sample(c(0,1,2), 1)
}

# sapply
results = sapply(c(1:1000), montystay)
print(sum(results == 1)/length(results))


# replicate
results2 = replicate(10000, montystay("dummy"))
print(sum(results2 ==1)/ length(results2))

# sample
x = sample(c(0,1,2), size=1)

sample(c(2), size = 1) # 2 , 1

# same
if( length(hostdoor) > 1)
  sample(hostdoor, 1)

sample(c(2), size = 1)
sample(2, size = 1)
sample(1:2, size = 1)

# print
cat("this is results", results, "\n")

# TRUE FALSE
# results = c(TRUE, FALSE, TRUE)
sum(results == TRUE) # it'll return counts of 'TRUE's.
sum(results)

