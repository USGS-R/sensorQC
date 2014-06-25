#'@title Creates flag vector based on input data
#'@description 
#'Creates flag vector with codes and methods according to params list.  \cr
#'
#'
#'@param data.in a data.frame with columns for DateTime and sensor.obs
#'@param sqc a sqc object with valid processing parameter names and associated values
#'@param verbose a boolean for diagnostic prints to workspace
#'@return a vector of flags of length equal to number of rows in data.in
#'@keywords methods
#'@author
#'Jordan S. Read
#'@examples 
#'dates <- seq(as.POSIXct('1999-01-01'),by=1,length.out=14)
#'values <- c(runif(12,2,4),NA,NA)
#'data.in <- data.frame("DateTime"=dates,"sensor.obs"=values)
#'simple.sqc <- list(outlier_removal=list(list(expression="x == 999999",type="error_code",description="logger error code"),
#'              list(expression='is.na(x)',type='error_code',description='missing data')))
#'
#'build_flags(data.in,sqc=simple.sqc)
#'@export

build_flags <- function(data.in,sqc,verbose=TRUE){
  
  # creates flag array based in data.in and parameters
  num.test <- length(sqc$outlier_removal)
  num.time <- nrow(data.in)
  flags.bool <- matrix(nrow = num.time, ncol = num.test)
  for (i in 1:length(sqc$outlier_removal)){
    flag.type <- as.character(sqc$outlier_removal[[i]]$type)
    expression <- as.character(sqc$outlier_removal[[i]]$expression)
    flags <- flag_wrap(flag.type,data.in,expr=expression,verbose)
    flags.bool[, i] <- flags
  }
  
  data.flags <- compress_flags(flags.bool)
  return(data.flags)
  
}

#'compresses boolean flags into int matrix padded with NAs
#'flags are compressed because it is assumed that they don't happen incredibly frequently
#'example: flag.bool <- matrix(nrow=3,ncol=4,data=c(F,F,F,F,F,F,T,T,F,F,F,F))
compress_flags <- function(flag.bool){
  # find longest j dimension of T
  num.row <- max(colSums(flag.bool))
  num.col <- ncol(flag.bool)
  data.flags <- matrix(NA_integer_, nrow = num.row, ncol=num.col)
  grab.idx <- seq_len(nrow(flag.bool))
  for (i in 1:num.col){
    num.use <- sum(flag.bool[, i])
    data.flags[seq_len(num.use), i] <- grab.idx[flag.bool[, i]]
  }
  return(data.flags)
}

unique_flags <- function(comp.flags){
  un.flags <- sort(unique(comp.flags[!is.na(comp.flags)]))
  return(un.flags)
}