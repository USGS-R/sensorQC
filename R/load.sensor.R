
#'@title Loads sensor data into data.frame
#'@description 
#'loads sensor data into data.frame according to the file format specified.  \cr
#'
#'@param filename a string
#'@param format a string which matches a valid sensor data format
#'@param date.type a string which represents a valid date format type
#'@return A data.frame with DateTime and values
#'@keywords methods
#'@author
#'Jordan S. Read
#'@examples 
#'filename <- '../examples/test_data.txt'
#'data.out <- load.sensor(filename,format="wide_burst", date.type="%Y-%m-%d %H:%M")
#'print(data.out)
#'@export
load.sensor <- function(filename='../examples/test_data.txt',format="wide_burst",date.type){
  
  if (format=="wide_burst"){
    data.out <- read.wide_burst(filename,date.type)
  } else{
    stop(paste('Sensor data format "',format,'" not supported',sep=''))
  }
  
  return(data.out)
}

read.wide_burst <- function(filename,date.type){
  # tab delimited with 4 header lines
  
  num.head <- 4
  t.step <- 1 #seconds, time between samples on the same row
  delim <- '\t'
  c <- file(filename,"r") #
  
  
  fileLines <- readLines(c)
  close(c)
  cat('num lines:');cat(length(fileLines));cat('\n')
  data.out <- init.sensor(length.out=length(fileLines)*60)
  
  cnt = 1
  for (i in (num.head+1):length(fileLines)){
    # for each line, first val is dateTime, second is "record"
    line <- fileLines[i]
    line.vals <- strsplit(line,split=delim)
    date.1 <- as.POSIXct(strptime(line.vals[[1]][1],date.type))
    dat.vals <- line.vals[[1]][c(-1,-2)] # only values (no dates or record number)
    num.dat <- length(dat.vals)
    data.out$sensor.obs[cnt:(cnt+num.dat-1)] <- as.numeric(dat.vals)
    data.out$DateTime[cnt:(cnt+num.dat-1)] <- seq(from=date.1,by="secs",length.out=num.dat)
    
    cnt=cnt+num.dat    
  }
  
  rmv.i <- is.na(data.out$sensor.obs)
  data.out <- data.out[!rmv.i, ]
  
  # should we also return metadata?
  return(data.out)
}
