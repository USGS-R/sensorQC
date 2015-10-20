#' read in a sensor or configuration file
#' 
#' read from file and create sensor or qconfig object
#' 
#' @param file filename
#' @param \dots additional arguments passed to file read methods
#' @param format format for file
#' @param date.format data format of file
#' @rdname read
#' 
#' @examples 
#' qconfig = read(system.file('extdata','pedro.yml', package = 'sensorQC'))
#' @importFrom tools file_ext
#' @importFrom yaml yaml.load_file
#' @export
read <- function(file, ...){
  
  if (!file.exists(file))
    stop(file, " doesn't exist. Check file and try again")
  
  if (file_ext(file) == 'yaml' || file_ext(file) == 'yml')
    read.config(file, ...)
  else
    read.default(file, ...)
}


read.config <- function(file, ...){
  
  
  sqc <- suppressWarnings(yaml.load_file(file))
  num.types <- length(sqc)
  for (k in 1:num.types){
    num.subs <- length(sqc[[k]])
    for (i in 1:num.subs){
      exp <- sqc[[k]][[i]][['expression']]
      if (!is.null(exp)){
        repl.lst <- exp.replace(exp)
        sqc[[k]][[i]][['expression']] <- repl.lst[['expression']]
        sqc[[k]][[i]][['alias']] <- repl.lst[['alias']]
      }
      date.form <- sqc[[k]][[i]][['date_type']]
      if (!is.null(date.form)){
        sqc[[k]][[i]][['date_type']] <- date.replace(date.form)
      }
    }
  }
  
  return(as.qconfig(sqc))
}

#' read default method 
#' 
#' read in sensor data
#' 
#' @rdname read
#' 
#' @export
read.default <- function(file, format, date.format, ...){
  
  x = do.call(paste0('read.',format), list(file=file, date.format=date.format, ...))
  
  return(sensor(x))
}

read.wide_burst <- function(file,date.format){
  # tab delimited with 4 header lines
  
  num.head <- 4
  t.step <- 1 #seconds, time between samples on the same row
  delim <- '\t'
  c <- file(file,"r") #
  
  
  fileLines <- readLines(c)
  close(c)
  cat('number of observations:');cat(length(fileLines)*30);cat('\n')
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
    date.1 <- as.POSIXct(strptime(line.vals[[1]][1],date.format))
    date.2 <- date.1+num.dat-1 # will be seconds
    date.vec[cnt:(cnt+num.dat-1)] <- seq(from=date.1,to=date.2,length.out=num.dat)#by="secs"
    
    cnt=cnt+num.dat    
  }
  date.vec <- head(date.vec,cnt-1)
  sens.vec <- head(sens.vec,cnt-1)
  data.out <- data.frame('DateTime'=date.vec, 'x'=sens.vec)
  
  # should we also return metadata?
  return(data.out)
}