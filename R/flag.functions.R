flag.wrap <- function(flag.type,data.in,expr){
  flags  <-  do.call(match.fun(flag.type),list(data.in=data.in,expr=expr))  
  return(flags)
}
  
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
  vals <- list('x'=as.numeric(data.in$sensor.obs))
  flags <- generic.sqc(vals = vals, expr)
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

stat_window <- function(data.in,expr){
  
  MAD <- median.absolute.deviation(data.in)
  CV <- coefficient.of.variation(data.in)
  
  vals <- list("MAD"=MAD,"CV"=CV)
  flags <- generic.sqc(vals,expr)
  return(flags)
}

#'@title generic function evaluator for sqc formats
#'@author
#'Jordan S. Read
generic.sqc <- function(vals,expr){
  test <- parse(text = expr)
  
  if (!is.list(vals)){
    vals <- list(x=vals)
    names(vals) <- substr(expr,1,1)
  }
    flags <- eval(test, envir=vals)
  return(flags)
}
  
call.mad <- function(data.in){
  b = 1.4826    # assuming a normal distribution
  # from Huber 1981:
  med.val  <-	median(data.in)					# median of the input data
  abs.med.diff	<-	abs(data.in-med.val)	# absolute values minus med
  abs.med	<-	median(abs.med.diff)			# median of these values
  
  MAD  <-	b*abs.med
  
  MAD.normalized <- abs.med.diff/MAD # division by zero
  
  MAD.normalized[is.na(MAD.normalized)] = 0 # doesn't protect against NAs that enter in data.in
  return(MAD.normalized)
}
#'@title median absolute deviation outlier test
#'@param \code{data.in} a \code{sensorQC} data.frame.
#'@return a vector of MAD normalized values relative to an undefined rejection criteria (usually 2.5 or 3).
#'@keywords MAD
#'@author
#'Jordan S. Read
median.absolute.deviation  <-  function(data.in){
  # does this method have to be public?	
  # what is the underlying distribution? (important for assigning "b")
  if (is.data.frame(data.in)){
    if (!"block.ID" %in% names(data.in)){stop("MAD can only accept numeric data, or a data.frame with the block.ID column for windowed data")}
    MAD.out <- vector(length=nrow(data.in))
    un.win <- unique(data.in$block.ID)
    
    for (i in 1:length(un.win)){
      win.i <- un.win[i]
      val.i <- data.in$block.ID == win.i
      MAD.out[val.i] = call.mad(data.in$sensor.obs[val.i])
    }
    return(MAD.out)
  } else {
    return(call.mad(data.in))
  }
  
}
call.cv <- function(data.in){
  CV <- 100*sd(data.in)/mean(data.in)
  return(CV)
}  
coefficient.of.variation <- function(data.in){
  
  
  if (is.data.frame(data.in)){
    if (!"block.ID" %in% names(data.in)){stop("CV can only accept numeric data, or a data.frame with the block.ID column for windowed data")}
    CV.out <- vector(length=nrow(data.in))
    un.win <- unique(data.in$block.ID)
    
    for (i in 1:length(un.win)){
      win.i <- un.win[i]
      val.i <- data.in$block.ID == win.i
      CV.out[val.i] = call.mad(data.in$sensor.obs[val.i])
    }
    return(CV.out)
  } else {
    return(call.cv(data.in))
  }
  
  
}

