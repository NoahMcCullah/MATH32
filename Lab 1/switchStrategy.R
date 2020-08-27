hostReveal = 0
experiments = 10000
doorNum = 3

montyswitch = function(i)
{
  doors = c(1:doorNum)
  carDoor = sample(doors,1)
  playerDoor1 = sample(doors, 1)
  
  hostDoors = doors[-c(playerDoor1, carDoor)]
  if(length(hostDoors) > 1){
    hostReveal = sample(hostDoors, 1)
  }
  else{
    hostReveal = hostDoors
  }
  
  finalDoors = doors[-c(hostReveal, playerDoor1)]
  if(length(finalDoors) > 1){
    playerDoor2 = sample(finalDoors, 1)
  }
  else{
    playerDoor2 = finalDoors
  }
  
  if(carDoor == playerDoor2){
    return(TRUE)
  }
  else{
    return(FALSE)
  }
}

results = sapply(FUN = montyswitch, X = c(1:experiments))
print(mean(results))