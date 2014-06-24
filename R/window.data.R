#'@title window sensorQC data
#'@name window_data
#'@description 
#'Breaks up time series data into window chunks.  \cr
#'@param data.in a data.frame of time series data
#'@param method A string. "auto" or "manual" supported.
#'@param window numeric, in seconds, specifying the window time width
#'@return a list of time series data and indices for breaks
#'@keywords window
#'@author
#'Jordan S. Read
#'@export
window_data <- function(data.in,method="auto",window=NULL){
  
  # breaks up data into time-windowed chunks
  # returns a list of breaks
  # add optional method to slice and dice?
  
  if (method=='auto'){
    windowed.data <- auto.chunk.time(data.in)
  }
  
  
  return(windowed.data)
}

auto.chunk.time <- function(data.in){
  
  # finds natural breaks in time sequence of data
  
  t.steps <- as.numeric(diff(data.in$DateTime))
  ###### re-write this!!
  MAD.norm <- median.absolute.deviation(data.in=t.steps) # deal with NAs?
  break.i <- MAD.norm > 2.5
  
  block.df <- data.frame("block.ID"=vector(length=nrow(data.in))*0) 
  
  windowed.data <- cbind(data.in,block.df)
  blck.i <- 1
  for (j in 1:(nrow(data.in)-1)){
    windowed.data$block.ID[j]=blck.i # this is slow, should bind at end of call
    if (break.i[j]){
      blck.i = blck.i+1
    }
  }
  
  windowed.data$block.ID[j+1]=blck.i
  
  return(windowed.data)
}