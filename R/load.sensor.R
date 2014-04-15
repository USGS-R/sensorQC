load.sensor <- function(filename,format='Pellerin'){
  
  
}

read.pellerin <- function(filename='../data/test_data.txt'){
  # tab delimited with 4 header lines
  
  num.head <- 4
  c <- file(filename,"r") #
  
  
  fileLines <- readLines(c)
  close(c)
  
  blank.vec <- vector(length=(length(fileLines)*60)) # will be too large, trim later
  blank.date <- rep(as.Date('1900-01-01'),(length(fileLines)*60))
  data.out <- data.frame("DateTime"=blank.date,"sensor.obs"=blank.vec)
  
  
  
  # should we also return metadata?
  return(data.out)
}