source("Function.R")
source("Coordinate.R")
source("StatCalc.R")
library(ggplot2)

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
      xList[[i]] = x
      yList[[i]] = trueFunc$evaluate(x) + rnorm(1)*trueFunc$getSlope()
    }
    myList = list(xList, yList)
    return(myList)
  }
  )

controller = Controller$new()
trueFunc = controller$generateRandomFunction()
data = controller$generateCoordinates(1000, trueFunc)
xList = data[[1]]
yList = data[[2]]
plot(unlist(xList), unlist(yList))
calculator = StatCalc$new()
approxFunc = controller$generateRandomFunction()
calculator$meanSquaredError(xList, yList, approxFunc)


