expNum = 10000
doorNum = 3

montystay = function(i)
{
  doors = c(1:doorNum)
  carDoor = sample(doors, 1)
  playerDoor = sample(doors, 1)
  
  if(carDoor == playerDoor){
    return(1)
  }
  else{
    return(0)
  }
}

results = sapply(results, FUN = montystay)

print(mean(results))