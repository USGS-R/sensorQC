#'@title window sensorQC data
#'@name window_data
#'@description 
#'Breaks up time series data into window chunks.  \cr
#'@param x a data.frame of time series data
#'@param type numeric, in seconds, specifying the window time width. or 
#'"auto" to automatically window data
#'@return a list of time series data and indices for breaks
#'@keywords window
#'@author
#'Jordan S. Read
#'@export
window.sensor<- function(x,  type, ...){
  
  # breaks up data into time-windowed chunks
  # returns a list of breaks
  # add optional method to slice and dice?
  
  if (type=='auto'){
    windowed.data <- auto.chunk.time(x$sensor)
  } else {
    windowed.data <- manual.chunk.time(x$sensor, type = type)
  }
  
  return(sensor(windowed.data))
}

#' @importFrom stats window
auto.chunk.time <- function(data.in){
  
  # finds natural breaks in time sequence of data
  data.in = as.data.frame(data.in[1:2])
  t.steps <- as.numeric(diff(data.in$DateTime))
  ###### re-write this!!
  MAD.norm <- call.mad(t.steps) # deal with NAs?
  break.i <- MAD.norm > 2.5
  
  block.int = vector(mode="integer",length=nrow(data.in))
  
  blck.i <- 1
  for (j in 1:(nrow(data.in)-1)){
    block.int[j]=blck.i
    if (break.i[j]){
      blck.i = blck.i+1
    }
  }
  
  block.df <- data.frame("windows"=block.int) 
  windowed.data <- cbind(data.in,block.df)
  
  windowed.data[['windows']][j+1]=blck.i
  
  return(windowed.data)
}

manual.chunk.time <- function(data.in, type){
  
  stop('manual window not yet supported')
  return()
}