#'@title Creates flag vector based on input data
#'@description 
#'Creates flag vector with codes and methods according to params list.  \cr
#'
#'
#'@param data.in a data.frame with columns for DateTime and sensor.obs
#'@param sqc a sqc object with valid processing parameter names and associated values
#'@param verbose a boolean for diagnostic prints to workspace
#'@return a vector of flags of length equal to number of rows in data.in
#'@keywords flags
#'@author
#'Jordan S. Read
#'@examples 
#'dates <- seq(as.POSIXct('1999-01-01'),by=1,length.out=14)
#'values <- c(runif(12,2,4),NA,NA)
#'data.in <- data.frame("DateTime"=dates,"sensor.obs"=values)
#'simple.sqc <- list(outlier_removal=list(expression="x == 999999",type="error_code",description="logger error code"))
#'
#'build.flags(data.in,sqc=simple.sqc)
#'@export

build.flags <- function(data.in,sqc,verbose=TRUE){
  
  # creates flag array based in data.in and parameters
  data.flags <- vector(length=nrow(data.in)) # vector of zeros
  for (i in 1:length(sqc)){
    flag.type <- as.character(sqc[[i]]$type)
    expression <- as.character(sqc[[i]]$expression)
    flags <- flag.wrap(flag.type,data.in,expr=expression,verbose)
    data.flags <- data.flags | flags
  }
  
  return(data.flags)
  
}