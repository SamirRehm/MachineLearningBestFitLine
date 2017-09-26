StatCalc <- setRefClass("StatCalc")
StatCalc$methods(
  meanSquaredError = function(xList, yList, line) {
    sumSquaredError = 0
    for(i in 1:length(data)) {
      approximateY = line$evaluate(xList[[i]])
      squaredError = (approximateY - yList[[i]])^2
      sumSquaredError = squaredError + sumSquaredError
    }
    print(sumSquaredError/length(data))
    return(sumSquaredError/length(data))
    
  }
)