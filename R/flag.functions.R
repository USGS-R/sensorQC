#'@title threshold tests according to expected format
#'@details a \code{sensorQC} function for creating flags based on threshold exceedence.\cr 
#'
#'@param \code{data.in} name of config file (proceeding *.sqc extension).
#'@param \code{expr} a valid expression as a string
#'@return a vector of booleans for flags for this test
#'@keywords threshold
#'@author
#'Jordan S. Read
threshold <- function(data.in,expr='x > 99'){
  test <- parse(text = expr)
  data.list <- list(x=data.in$sensor.obs)
  names(data.list) <- substr(expr,1,1)
  flags <- eval(test, envir=data.list)
  return(flags)
}