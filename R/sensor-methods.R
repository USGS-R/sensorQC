
as.data.frame.sensor <- function(sensor){
  class(sensor) <- 'data.frame' # make this more robust
  return(sensor)
}

#' @export
`[.sensor` <- function(x, ...){
  #NextMethod('[')
  `[.data.frame`(as.data.frame(x), ...)
}

times <- function(x){
  UseMethod('times')
}

#' @export
times.sensor <- function(x){
  x[,1]
}

values <- function(x){
  UseMethod('values')
}

#' @export
values.sensor <- function(x){
  x[,-1]
}

windows <- function(x) UseMethod('windows')

windows.sensor <- function(x){
  NULL # implement later
}