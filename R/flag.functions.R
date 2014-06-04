#'@title threshold tests according to expected format
#'@details a \code{sensorQC} function for creating flags based on threshold exceedence.\cr 
#'
#'@param \code{data.in} a \code{sensorQC} data.frame.
#'@param \code{expr} a valid expression as a string
#'@return a vector of booleans for flags for this test
#'@keywords threshold
#'@author
#'Jordan S. Read
threshold <- function(data.in,expr='x > 99'){
  flags <- generic.sqc(vals = data.in$sensor.obs,expr)
  return(flags)
}

#'@title tests for value equal to a known error code
#'@details a \code{sensorQC} function for creating flags based on error code matches.\cr 
#'
#'@param \code{data.in} a \code{sensorQC} data.frame.
#'@param \code{expr} a valid expression as a string (e.g., x==-999)
#'@return a vector of booleans for flags for this test
#'@keywords error_code
#'@author
#'Jordan S. Read
error_code <- function(data.in,expr='x == -999'){
  flags <- generic.sqc(vals = data.in$sensor.obs,expr)
  return(flags)
}

#'@title tests for sequentially repeated values
#'@details a \code{sensorQC} function for creating flags based on repeated sequential values.\cr 
#'
#'@param \code{data.in} a \code{sensorQC} data.frame.
#'@param \code{expr} a valid expression as a string (e.g., n>10)
#'@return a vector of booleans for flags for this test
#'@keywords persistent
#'@author
#'Jordan S. Read
persistent <- function(data.in,expr='n > 10'){  
  tmp <- rle(data.in$sensor.obs)
  vals <- rep(tmp$lengths,times = tmp$lengths)
  flags <- generic.sqc(vals=vals,expr=expr)
  return(flags)
}

stat_window <- function(windowed.data,expr){
  
  return(flags)
}

#'@title generic function evaluator for sqc formats
#'@author
#'Jordan S. Read
generic.sqc <- function(vals,expr){
  test <- parse(text = expr)
  
  if (!is.list(vals)){
    data.list <- list(x=vals)
    names(data.list) <- substr(expr,1,1)
  }

  flags <- eval(test, envir=data.list)
  return(flags)
}
