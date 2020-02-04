fun <- function(x){
  return(1 + 2 * x - x * x)
}

inertia = .9
coeffLb = 2
coeffGb = 2
n = 100

randVar1 = runif(n,-5,5)
randVar2 = runif(n,-5,5)

globalBestFitness = -9999
currentPosition = runif(n,-5,5)
velocity = runif(n,-.5,.5)

iter = 200

currentFitness = fun(currentPosition)
localBestPosition = currentPosition
localBestFitness = currentFitness

for (inertia in seq(.9,.4,-(.9-.4)/iter)) {
  max = max(currentFitness)
  ind = which(currentFitness == max)[1]
  
  if(max > globalBestFitness){
    globalBestFitness = max
    gbp = currentPosition[ind]
  }
  
  velocity = inertia * velocity + randVar1 * coeffLb * (currentPosition - localBestPosition) + randVar2 * coeffGb * (currentPosition - gbp)
  currentPosition = currentPosition + velocity 
  
  currentFitness = fun(currentPosition)
  i = 1
  for (f in currentFitness) {
    
    if(f > localBestFitness[i]){
      localBestFitness[i] = f
      localBestPosition[i] = currentPosition[i]
    }
    i = i + 1
    
  }
}
print(globalBestFitness)
