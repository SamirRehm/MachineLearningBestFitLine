source("Function.R")
source("Coordinate.R")
source("StatCalc.R")
library(ggplot2)

Controller <- setRefClass("Controller")
Controller$methods(
  generateRandomFunction = function() {
    slope = runif(1) * 100
    yInt = runif(1) * 1000
    func = Func$new(slope = slope, yInt = yInt)
    return(func)
  },
  
  generateCoordinates = function(numCoords, trueFunc) {
    xList = list(numCoords)
    yList = list(numCoords)
    for(i in 1:numCoords) {
      x = runif(1) * 100
      xList[[i]] = x
      yList[[i]] = trueFunc$evaluate(x)
    }
    myList = list(xList, yList)
    return(myList)
  }
  )

controller = Controller$new()
trueFunc = controller$generateRandomFunction()
trueFunc$printFunction()
data = controller$generateCoordinates(10000, trueFunc)

xList = data[[1]]
yList = data[[2]]

plot(unlist(xList), unlist(yList))

calculator = StatCalc$new()
approxFuncSlope = list(100)
approxFuncInt = list(100)
ErrorList = list(100)
for(j in 1:9)  {

  approxFunc = controller$generateRandomFunction()
  approxFuncSlope[[j]] = approxFunc$slope
  approxFuncInt[[j]] = approxFunc$yInt

  ErrorList[[j]] = calculator$meanSquaredError(xList, yList, approxFunc)
}
Slope2 = unlist(approxFuncSlope)
ErrorList2= unlist(ErrorList)
yIntercept = unlist(approxFuncInt)

model <- lm(ErrorList2 ~ Slope2 + I(Slope2*Slope2) + I(Slope2*yIntercept) + yIntercept + I(yIntercept^2))
summary(model)
coefficients = model$coefficients
print(coefficients)
f <- function(x) (coefficients[3]*x[1]^2 + coefficients[2] * x[1] + coefficients[5] * x[2] + coefficients[1] + coefficients[4] * x[1] * x[2] + coefficients[6] * x[2] * x[2])
print(f)
result1 = optim(c(50,50), f, lower = c(0,0), upper = c(100,1000),method="L-BFGS-B" )$par
result = Func$new(yInt = result1[2], slope = result1[1])
result$printFunction()
trueFunc$printFunction()
calculator$meanSquaredError(xList, yList, result)
