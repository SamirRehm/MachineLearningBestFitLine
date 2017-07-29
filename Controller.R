source("Function.R")
source("Coordinate.R")

Controller <- setRefClass("Controller")
Controller$methods(
  generateRandomFunction = function() {
    slope = runif(1) * 100
    yInt = runif(1) * 100
    func = Func$new(slope = slope, yInt = yInt)
    func$printFunction()
    return(func)
  },
  
  generateCoordinates = function(numCoords, trueFunc) {
    xList = list(numCoords)
    yList = list(numCoords)
    for(i in 1:numCoords) {
      x = runif(1) * 100
      coord = Coordinate$new(x = x, y = trueFunc$evaluate(x) + rnorm(1)*trueFunc$getSlope())
      xList[[i]] = coord$getXCoordinate()
      yList[[i]] = coord$getYCoordinate()
    }
    myList = list(xList, yList)
    return(myList)
  }
  )

controller = Controller$new()
trueFunc = controller$generateRandomFunction()
data = controller$generateCoordinates(100, trueFunc)
xList = data[[1]]
yList = data[[2]]
system.time(plot(xList, yList))
