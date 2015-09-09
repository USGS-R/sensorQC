
MAD.values <- function(vals, b = 1.4826){
  # b: assuming a normal distribution
  # from Huber 1981:
  med.val  <-	median(vals)					# median of the input data
  abs.med.diff	<-	abs(vals-med.val)	# absolute values minus med
  abs.med	<-	median(abs.med.diff)			# median of these values
  
  MAD  <-	b*abs.med
  
  MAD.normalized <- abs.med.diff/MAD # division by zero
  
  MAD.normalized[is.na(MAD.normalized)] = 0 # doesn't protect against NAs that enter in data.in
  return(MAD.normalized)
}

MAD.windowed <- function(vals, windows){
  stopifnot(length(vals) == length(windows))
  
  # what is the underlying distribution? (important for assigning "b")
  
  MAD.out <- vector(length=length(vals))
  un.win <- unique(windows)
  
  for (i in 1:length(un.win)){
    win.i <- un.win[i]
    val.i <- windows == win.i
    MAD.out[val.i] = MAD.values(vals[val.i])
  }
  return(MAD.out)
}
#'@title median absolute deviation outlier test
#'@name MAD
#'@aliases MAD
#'@aliases median.absolute.deviation
#'@param x values
#'@param windows vector of equal length to x specifying windows
#'@return a vector of MAD normalized values relative to an undefined rejection criteria (usually 2.5 or 3).
#'@keywords MAD
#'@author
#'Jordan S. Read
#'@export
MAD <- function(x, w){
  
  if(missing(w))
    MAD.values(x)
  else
    MAD.windowed(x,w)
  
}



call.cv <- function(data.in){
  CV <- 100*sd(data.in)/mean(data.in)
  CV <- rep(CV,length(data.in))
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
      CV.out[val.i] = call.cv(data.in$sensor.obs[val.i])
    }
    return(CV.out)
  } else {
    return(call.cv(data.in))
  }
  
  
}
