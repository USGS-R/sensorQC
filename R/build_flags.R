
#'@title Creates flag vector based on input data
#'@description 
#'Creates flag vector with codes and methods according to params list.  \cr
#'
#'
#'@param data.in a data.frame with columns for DateTime and sensor.obs
#'@param sqc a sqc object with valid processing parameter names and associated values
#'@param verbose a boolean for diagnostic prints to workspace
#'@param compress a boolean for whether flags are compressed
#'@param flatten a boolean for whether flags are flat (1D vector of 'any') or n by m matrix
#'@return a vector of flags of length equal to number of rows in data.in
#'@keywords methods
#'@author
#'Jordan S. Read
#'@examples 
#'dates <- seq(as.POSIXct('1999-01-01'),by=1,length.out=14)
#'values <- c(runif(12,2,4),NA,NA)
#'data.in <- data.frame("DateTime"=dates,"sensor.obs"=values)
#'simple.sqc <- list(list(expression="x == 999999",type="error_code",description="logger error code"),
#'              list(expression='is.na(x)',type='error_code',description='missing data'))
#'
#'flag.sensor(data.in, "x == 999999")
#'build_flags(data.in,sqc=simple.sqc, compress = FALSE, flatten = TRUE)
#'@export
flag <- function(x, flag.defs, ...){
  UseMethod('flag')
}

#' @export
flag.data.frame <- function(x, flag.defs, ...){
  UseMethod('flag',sensor(x))
}

#' @export
flag.sensor <- function(sensor, flag.defs, ...){
  
  flagged = flagged(sensor, flag.defs, ...)
  flags = flags(flagged)
  sensor = sensor(flagged)
  for (i in seq_len(length(flags))){
    flag.type <- as.character(flags[[i]]$type)
    expression <- as.character(flags[[i]]$expression)
    alias <- as.character(flags[[i]]$alias)
    flags <- flag_wrap(data,expr=expression,alias=alias, ...)
    
  }
  flags.bool[is.na(flags.bool)] = TRUE
  if (compress){
    flags.bool <- compress_flags(flags.bool)
  }
  
  return(flags.bool)
  
}


flagged <- function(x, ...){
  UseMethod('flagged')
}

flagged.sensor <- function(sensor, flag.defs, ...){
  
  add_indices <- function(x) {
    for (i in seq_len(length(x))){
      x[[i]]=append(x[[i]],list('flag.i'=c()))
    }
    x
  }
  flagged <- list('sensor'=sensor, 
                  flags = lapply(flagged(flag.defs, ...), add_indices))
  class(flagged) <- 'flagged'
  return(flagged)
}

flagged.data.frame <- function(x, ...){
  UseMethod('flagged',sensor(x))
}

#' @export
flagged.character <- function(x, ...){
  flag.defs = append(list(x), list(...))
  list('inst'=lapply(flag.defs, function(x) list('expression'=x)))
}

#' @export
flagged.qconfig <- function(qconfig){
  list('inst'=qconfig[['outlier_removal']],
       'window'=qconfig[['block_stats']])
}

flags <-function(x){
  UseMethod('flags')
}

#' @export
flags.flagged <- function(flagged){
  flagged$flags
}
