#' @export
plot.sensor <- function(x, y=NULL, ...){
  args = expand.grid(...)
  # check that ylab and xlab aren't in ...
  plot.x = x$sensor$times
  plot.y = x$sensor$x
  if (!is.null(flags(x))){
    flag.i <- sort(unique(unlist(sapply(flags(x), function(x) x$flag.i))))
    plot.x = plot.x[-flag.i]
    plot.y = plot.y[-flag.i]
    
  }
  
  plot(plot.x, plot.y, ylab='sensor', xlab='DateTime')
}