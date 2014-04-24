init.sensor <- function(length.out){
  
  blank.date <- rep(as.POSIXct('1900-01-01'),length.out)
  blank.vec <- vector(length=length.out)*NA
  sensor.out <- data.frame("DateTime"=blank.date,"sensor.obs"=blank.vec)
 
  return(sensor.out)
}