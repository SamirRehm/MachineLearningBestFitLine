Func <- setRefClass("Func", fields = list(yInt = "numeric", slope = "numeric"))

Func$methods(
  
  setYInt = function(yInt)  {
    .self$yInt = yInt
  },
  
  setSlope = function(slope) {
    .self$slope = slope
  },
  
  getYInt = function()  {
    return(.self$yInt)
  },
  
  getSlope = function()  {
    return(.self$slope)
  },
  
  evaluate = function(x)  {
    return(.self$slope * x + .self$yInt )
  },
  
  printFunction = function() {
    print(paste("The function is", .self$slope, "x +", .self$yInt))
  }
)
