#' @export
sensor <- function(x, flag.defs, ...){
  UseMethod('sensor')
}

#' @export
sensor.data.frame <- function(x, flag.defs = NULL, ...){
  sensor = list(sensor=x, flags = flag.defs(flag.defs,...))
  class(sensor) <- 'sensor'
  return(sensor)
}

flag.defs <- function(x, ...){
  if (is.character(x)){
    flag.defs = append(list(x), list(...))
    return(lapply(flag.defs, function(x) list('expression'=x)))
  } else {
    flag.defs = append(x, list(...))
  }
    
  return(flag.defs)
}

#' @export
sensor.sensor <- function(sensor){
  sensor
}

