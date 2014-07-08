
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
  sens.vec <- vector(mode="numeric",length=length(fileLines)*60)
  date.vec <- rep(as.POSIXct('1900-01-01'),length(fileLines)*60)
  
  cnt = 1
  for (i in (num.head+1):length(fileLines)){
    # for each line, first val is dateTime, second is "record"
    line <- fileLines[i]
    line.vals <- strsplit(line,split=delim)
    dat.vals <- line.vals[[1]][c(-1,-2)] # only values (no dates or record number)
    num.dat <- length(dat.vals)
    sens.vec[cnt:(cnt+num.dat-1)] <- as.numeric(dat.vals)
    date.1 <- as.POSIXct(strptime(line.vals[[1]][1],date.type))
    date.2 <- date.1+num.dat-1 # will be seconds
    date.vec[cnt:(cnt+num.dat-1)] <- seq(from=date.1,to=date.2,length.out=num.dat)#by="secs"
    
    cnt=cnt+num.dat    
  }
  date.vec <- head(date.vec,cnt-1)
  sens.vec <- head(sens.vec,cnt-1)
  data.out <- data.frame('DateTime'=date.vec, 'sensor.obs'=sens.vec)
  
  # should we also return metadata?
  return(data.out)
}
