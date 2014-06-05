#'@title load in configuration file for sensorQC
#'#'@author
#'Jordan S. Read
#'@examples 
#'NULL
#'@export
block.stats <- function(windowed.data,data.flags=NULL,rmv.cv=TRUE){
  
  #exclude flags should come from params..
  #clean.data <- exclude.outliers(data.in=windowed.data,data.flags,exclude.flags,write.log=FALSE)
  if (!is.null(data.flags)){
    clean.data <- windowed.data[!data.flags, ]
  } else {
    clean.data <- windowed.data
  }
  
  
  un.blcks <- unique(clean.data$block.ID)

  block.out <- init.sensor(length.out=length(un.blcks),append=c("CV"))
  

  for (i in seq_len(length(un.blcks))){
    use.i <- which(clean.data$block.ID==un.blcks[i])
    block.out$DateTime[i] <- mean(clean.data$DateTime[use.i])
    block.out$sensor.obs[i] <- mean(clean.data$sensor.obs[use.i])
    block.out$CV[i] <- co.var(clean.data$sensor.obs[use.i])
  }
  
  if (rmv.cv){

    block.rmv <- call.mad(block.out$CV) > 3
    block.out = block.out[!block.rmv, ]
  }
  
  
  return(block.out)
}

co.var <- function(x) {
  100*sd(x)/mean(x)
}
