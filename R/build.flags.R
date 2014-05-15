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
  
  data.flags <- vector(length=nrow(data.in),mode="numeric") # vector of zeros
  # types of flagged data: NA=1, 999999=2, MAD=3, etc...
  # need a matching code function. Or summary function which parameterizes this
  
  
  # NA check
  rplc.val = 1
  data.flags[is.na(data.in$sensor.obs)] <- rplc.val
  
  # loggernet check
  rplc.val = 2
  data.flags[data.in$sensor.obs==999999] <- rplc.val
  
  # window data
  windowed.data <- window.data(data.in=data.in,method="auto")
  
  # MAD (if applicable) check
  rplc.val = 3
  un.blcks <- unique(windowed.data$block.ID)
  for (i in seq_len(length(un.blcks))){
    use.i <- which(windowed.data$block.ID==un.blcks[i] & data.flags==0) # ignore values that are already flagged
    check.dat <- windowed.data$sensor.obs[use.i]
    mad.flag <- get.outliers(check.dat,method='median.absolute.deviation',reject.criteria=2.5,na.rm=FALSE)
    data.flags[use.i[mad.flag]] <- rplc.val
  }
  
  # call window.data, then get.outliers, etc. *USE params*
  
  
  return(data.flags)
  
}