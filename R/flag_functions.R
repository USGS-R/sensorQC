calc_flags <- function(x, ...){
  UseMethod('calc_flags')
}

#' @importFrom dplyr mutate_
#' @importFrom lazyeval as.lazy
#' @export
calc_flags.sensor <- function(sensor, expr, which.flagged=TRUE){

  flags <- mutate_(sensor$sensor[-1], flags = lazyeval::as.lazy(expr, globalenv()))$flags
  
  if (!inherits(flags, 'logical'))
    stop(expr,' failed to generate booleans')
  #check for class of flags
  if (which.flagged)
    return(which(flags))
  else  
    return(flags)
}
