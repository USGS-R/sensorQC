#'@title Creates flag vector based on input data
#'@description 
#'Creates flag vector with codes and methods according to params list.  \cr
#'
#'@usage
#'data.flags <- build.flags(data.in,params)
#'
#'@param \code{data.in} a data.frame with columns for DateTime and sensor.obs
#'@param \code{sqc} a sqc object with valid processing parameter names and associated values
#'@return a vector of flags of length equal to number of rows in data.in
#'@keywords flags
#'@author
#'Jordan S. Read
#'@examples 
#'dates <- seq(as.POSIXct('1999-01-01'),by=1,length.out=14)
#'values <- c(runif(12,2,4),NA,NA)
#'data.in <- data.frame("DateTime"=dates,"sensor.obs"=values)
#'
#'build.flags(data.in,params=NULL)
#'@export

build.flags <- function(data.in,sqc){
  
  # creates flag array based in data.in and parameters
  data.flags <- vector(length=nrow(data.in)) # vector of zeros
  for (i in 1:nrow(sqc)){
    flag.type <- as.character(sqc$QAQC_type[i])
    expression <- as.character(sqc$expression[i])
    cat(flag.type);cat('\n')
    flags <- flag.generic(flag.type,data.in,expr=expression)
    data.flags <- data.flags | flags
  }
  
  return(data.flags)
  
}