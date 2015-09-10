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
  
  print(head(x$sensor[,1:2], max.row))
  cat('  ...\n')
  for (i in 1:length(flags(x))){
    cat(flags(x)[[i]]$expression,paste0('(',length(flags(x)[[i]]$flag.i),' flags)\n'))
  }
}
