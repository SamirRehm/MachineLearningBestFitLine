Coordinate <- setRefClass("Coordinate", fields = list(x = "double", y = "double"))

Coordinate$methods(
  getXCoordinate = function() {
    return(.self$x)
  },
  getYCoordinate = function() {
    return(.self$y)
  }
)
  