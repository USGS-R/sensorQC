
MAD.values <- function(vals, b = 1.4826){
  # b: assuming a normal distribution
  # from Huber 1981:
  u.i <- is.finite(vals)
  med.val  <-	median(vals[u.i])					# median of the input data
  abs.med.diff	<-	abs(vals-med.val)	# absolute values minus med
  abs.med	<-	median(abs.med.diff[u.i])			# median of these values
  
  MAD  <-	b*abs.med
  MAD.normalized = rep(NA,length(vals))
  MAD.normalized[u.i] <- abs.med.diff[u.i]/MAD # division by zero
  MAD.normalized[is.na(MAD.normalized)] <- 0
  return(MAD.normalized)
}

MAD.windowed <- function(vals, windows){
  stopifnot(length(vals) == length(windows))
  . <- '_dplyr_var'
  mad <- group_by_(data.frame(x=vals,w=windows), 'w') %>% mutate_(mad='sensorQC:::MAD.values(x)') %>% .$mad
  return(mad)
}
#'@title median absolute deviation outlier test
#'@name MAD
#'@aliases MAD
#'@aliases median.absolute.deviation
#'@param x values
#'@param w vector of equal length to x specifying windows
#'@return a vector of MAD normalized values relative to an undefined rejection criteria (usually 2.5 or 3).
#'@keywords MAD
#'@importFrom dplyr group_by_ mutate_ %>%
#'@author
#'Jordan S. Read
#'@export
MAD <- function(x, w){
  
  if(missing(w))
    MAD.values(x)
  else
    MAD.windowed(x,w)
  
}

#' function for checking persistent values
#' 
#' repeated values of a vector
#' @param x a numeric vector
#' @return a vector that provides a count of persistance
#' @export
persist <- function(x){
  tmp <- rle(x)
  rep(tmp$lengths,times = tmp$lengths)
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
