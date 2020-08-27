expNum = 1000
number = 3
hostReveal = 0
resultsStay = 0*c(1:expNum)
resultsSwitch = 0*c(1:expNum)
resStayNum = 0*c(1:expNum)
resSwitchNum = 0*c(1:number)

montystay = function(i)
{
  doors = c(1:i)
  carDoor = sample(doors, 1)
  playerDoor = sample(doors, 1)
  
  if(carDoor == playerDoor){
    return(1)
  }
  else{
    return(0)
  }
}

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

for(val in number){
  for(val2 in expNum){
    results[expNum] = montystay(val)
  }
  resStayNum[val] = mean(results)
  print(results)
}

plot(results, type = 'b', xlab = 'door number')

