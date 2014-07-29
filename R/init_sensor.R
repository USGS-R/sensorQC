init.sensor <- function(length.out,append=NULL,main.name='sensor.obs'){
  
  blank.date <- rep(as.POSIXct('1900-01-01'),length.out)
  blank.vec <- vector(length=length.out)*NA
  sensor.out <- data.frame("X1"=blank.date,"X2"=blank.vec)
  names(sensor.out) <- c("DateTime", main.name)  
  for (i in seq_len(length(append))){
    name.old <- names(sensor.out)
    sensor.out <- cbind(sensor.out,blank.vec)
    names(sensor.out) <- c(name.old,append[i])
  }
  
  return(sensor.out)
}