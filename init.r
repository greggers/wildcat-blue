if(!suppressWarnings(require(MCMCpack))) {
  
  install.packages("MCMCpack")
  
  library(MCMCpack)
  
} else {
  
  library(MCMCpack)
  
}



if(!suppressWarnings(require(car))) {
  
  install.packages("car")
  
  library(car)
  
} else {
  
  library(car)
  
}

if(!suppressWarnings(require(shiny))) {
  
  install.packages("shiny")
  
  library(shiny)
  
} else {
  
  library(shiny)
  
}