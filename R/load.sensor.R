load.sensor <- function(filename='../examples/test_data.txt',format='Pellerin'){
  
  if (format=='Pellerin'){
    data.out <- read.pellerin(filename)
  } else{
    stop(paste('Sensor data format "',format,'" not supported',sep=''))
  }
  
  return(data.out)
}

read.pellerin <- function(filename){
  # tab delimited with 4 header lines
  
  num.head <- 4
  t.step <- 1 #seconds, time between samples on the same row
  delim <- '\t'
  c <- file(filename,"r") #
  
  
  fileLines <- readLines(c)
  close(c)
  
  blank.vec <- vector(length=(length(fileLines)*60))*NA # will be too large, trim later
  blank.date <- rep(as.POSIXct('1900-01-01'),(length(fileLines)*60))
  data.out <- data.frame("DateTime"=blank.date,"sensor.obs"=blank.vec)
  
  cnt = 1
  for (i in (num.head+1):length(fileLines)){
    # for each line, first val is dateTime, second is "record"
    line <- fileLines[i]
    line.vals <- strsplit(line,split=delim)
    date.1 <- as.POSIXct(strptime(line.vals[[1]][1],"%m/%d/%Y %H:%M"))
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