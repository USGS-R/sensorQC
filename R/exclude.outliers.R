#'@title removes outliers according to flags
#'@description 
#'removes outliers according to flags.  \cr
#'
#'@usage
#'data.out <- exclude.outliers(data.in,data.flags,exclude.flags,write.log=TRUE)
#'
#'@param \code{data.in} a time series data.frame
#'@param \code{data.flags} a numeric vector (same length as ncols in data.in) containing data flags
#'@param \code{exclude.flags} filter for the type of flags to remove vs ignore
#'@param \code{write.log} boolean to determine whether log file is written
#'@return A data.frame with DateTime and values
#'@keywords outliers
#'@author
#'Jordan S. Read
#'@examples 
#'dates <- seq(as.POSIXct('1999-01-01'),by=1,length.out=12)
#'values <- runif(12,2,4)
#'data.in <- data.frame("DateTime"=dates,"sensor.obs"=values)
#'data.flags <- c(0,0,0,0,4,0,3,0,3,0,0,0)
#'exclude.flags <- c(1,4) # only exclude type 1 and type 4 flags
#'
#'exclude.outliers(data.in,data.flags,exclude.flags,write.log=F)
#'@export
exclude.outliers <- function(data.in,data.flags,exclude.flags,write.log=TRUE){
  
  # takes in data and data flags, gets rid of outliers, writes log file if applicable
  
  if (nrow(data.in) != length(data.flags)){
    stop("number of columns for data and flags must be equal")
  }
  
  rmv.i <- data.flags %in% exclude.flags 
  
  data.out <- data.in[!rmv.i, ]
  
  if (write.log){
    # call write log here..
  }
  
  return(data.out)
}