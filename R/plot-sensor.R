#' @export
plot.sensor <- function(x, y=NULL, ...){
  args = expand.grid(...)
  # check that ylab and xlab aren't in ...
  
  x <- clean(x, which='all',replace=NULL)

  
  plot(x$sensor$times, x$sensor$x, ylab='sensor', xlab='DateTime')
}