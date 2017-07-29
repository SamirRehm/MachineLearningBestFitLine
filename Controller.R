rm(list=ls())
Controller <- setRefClass("Controller", fields = list(age = "numeric"))
Controller$methods(
  update_age = function() {
    .self$age = age+1;
    .self$age
  }
  )

controller = Controller$new(age=5)
controller$age
controller$update_age()
ls()
