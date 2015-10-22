#' create a sensor object
#' 
#' sensor data
#' 
#' @param x a data.frame or sensor object
#' @param flag.defs definitions for flags. Functions that can be evaluated to T/F
#' @param \dots additional flag.defs
#' @export
sensor <- function(x, flag.defs, ...){
  UseMethod('sensor')
}

#' @export
sensor.data.frame <- function(x, flag.defs = NULL, ...){
  names(x) <- c('times','x','w')[seq_len(length(names(x)))]
  sensor = list(sensor=x)
  flags = define_flags(flag.defs,...)
  if (!is.null(flags))
    sensor <- append(sensor, list(flags = flags))
  class(sensor) <- 'sensor'
  return(sensor)
}

#' @export
sensor.double <- function(x, flag.defs = NULL, ...){
  x = data.frame(times = rep(NA,length(x)), x = x)
  
  sensor(x, flag.defs = NULL, ...)
}


define_flags <- function(x, ...){
  
  # x is null and there is nothing else
  if (is.null(x) & !length(list(...)))
    return(NULL)
  
  if (length(list(...))){
    if (is.character(...))
      b = lapply(..., function(x) list('expression'=x))
    else
      b = list(...)
  } else {
    b = NULL
  }
  
  if (!is.null(x)){
    if (is.character(x))
      x = lapply(x, function(x) list('expression'=x))
  }
  return(append(x, b))
  
}

#' @export
sensor.sensor <- function(x, flag.defs = NULL, ...){
  sensor(x$sensor, flag.defs=flags(x), c(flag.defs, ...))
}

#' @export
print.sensor <- function(x, ..., max.row=15){
  cat('object of class "sensor"\n')
  if (!all(is.na(x$sensor$times)))
    print(head(x$sensor[,1:2], max.row))
  else
    print(head(x$sensor[2], max.row))
  if (nrow(x$sensor) > max.row)
    cat('  ...')
  cat('\n')
  for (i in seq_len(length(flags(x)))){
    cat(flags(x)[[i]]$expression,paste0('(',length(flags(x)[[i]]$flag.i),' flags)\n'))
  }
}
