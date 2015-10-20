#' @export
plot.sensor <- function(x, y=NULL, ...){
  args = expand.grid(...)
  # check that ylab and xlab aren't in ...
  if (is.null(flags(x))){
    plot(x$sensor$times, x$sensor$x, ylab='sensor', xlab='xlab')
  } else {
    flag.i <- sort(unique(unlist(sapply(flags(x), function(x) x$flag.i))))
    plot(x$sensor$times[-flag.i], x$sensor$x[-flag.i], ylab='sensor', xlab='xlab')
  }
  
  
}