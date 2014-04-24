init.sensor <- function(length.out,append=NULL){
  
  blank.date <- rep(as.POSIXct('1900-01-01'),length.out)
  blank.vec <- vector(length=length.out)*NA
  sensor.out <- data.frame("DateTime"=blank.date,"sensor.obs"=blank.vec)
  
  for (i in seq_len(length(append))){
    name.old <- names(sensor.out)
    sensor.out <- cbind(sensor.out,blank.vec)
    names(sensor.out) <- c(name.old,append[i])
  }
  
  return(sensor.out)
}