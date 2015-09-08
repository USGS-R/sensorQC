
as.data.frame.sensor <- function(sensor){
  class(sensor) <- 'data.frame' # make this more robust
  return(sensor)
}

#' @export
`[.sensor` <- function(x, ...){
  `[.data.frame`(as.data.frame(x), ...)
}

times <- function(x){
  UseMethod('times')
}

#' @export
times.sensor <- function(x){
  x$sensor[,1]
}

values <- function(x){
  UseMethod('values')
}

#' @export
values.sensor <- function(x){
  x$sensor[,2]
}

windows <- function(x) UseMethod('windows')

windows.sensor <- function(x){
  x$sensor[['windows']]
}


flags <-function(x){
  UseMethod('flags')
}

#' @export
flags.sensor <- function(sensor){
  sensor$flags
}
