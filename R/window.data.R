window.data <- function(data.in,method="auto",window=30){
  
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
  break.i <- get.outliers(data.in=t.steps,method='median.absolute.deviation',reject.criteria=2.5,na.rm=T)
  
  block.df <- data.frame("block.ID"=vector(length=nrow(data.in))*0)
  
  windowed.data <- cbind(data.in,block.df)
  blck.i <- 1
  for (j in 1:(nrow(data.in)-1)){
    windowed.data$block.ID[j]=blck.i
    if (break.i[j]){
      blck.i = blck.i+1
    }
  }
  
  return(windowed.data)
}