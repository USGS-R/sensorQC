#' @export
sensor <- function(data){
  UseMethod('sensor')
}

#' @export
sensor.data.frame <- function(data){
  class(data) <- 'sensor'
  return(data)
}

#' @export
sensor.list <- function(x){
  data <- x$sensor
  return(sensor(data))
}

#' @export
sensor.flagged <- function(x){
  UseMethod('sensor', list())
}

#' @export
sensor.sensor <- function(sensor){
  sensor
}