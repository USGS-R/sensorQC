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
  flags <- generic.sqc(vals = data.in$sensor.obs,expr)
  return(flags)
}

generic.sqc <- function(vals,expr){
  test <- parse(text = expr)
  data.list <- list(x=vals)
  names(data.list) <- substr(expr,1,1)
  flags <- eval(test, envir=data.list)
  return(flags)
}

#'@title tests for value equal to a known error code
#'@details a \code{sensorQC} function for creating flags based on error code matches.\cr 
#'
#'@param \code{data.in} name of config file (proceeding *.sqc extension).
#'@param \code{expr} a valid expression as a string (e.g., n==10)
#'@return a vector of booleans for flags for this test
#'@keywords error_code
#'@author
#'Jordan S. Read
error_code <- function(data.in,expr='x == -999'){
  flags <- generic.sqc(vals = data.in$sensor.obs,expr)
  return(flags)
}

persistent <- function(data.in,expr='n == 10'){
  
  vals <- data.in$sensor.obs
  test <- parse(text = expr)
  n.vals <- length(vals)
  vec.persist <- vector(length=n.vals)
  # will fail if n.vals < 2...
  for (j in 2:n.vals){
    
  }
  
  
  data.list <- list(x=data.in$sensor.obs)
  names(data.list) <- substr(expr,1,1)
  flags <- eval(test, envir=data.list)
  return(flags)
}