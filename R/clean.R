#' clean a sensor or a flagged sensor
#' 
#' clean a sensor or a flagged sensor
#' 
#' @param x sensor object
#' @param which flags to replace
#' @param \dots additional flags to use (passed to flag(x,...))
#' @param replace replacement value for flagged indices. 
#' NULL removes the flagged row
#' @rdname clean
#' 
#' @examples 
#' data = c(999999, 1,2,3,4,2,3,4)
#' sensor = flag(data, 'x > 9999')
#' clean(sensor)
#' clean(data, 'x > 9999', 'persist(x) > 10', 'MAD(x) > 3', replace=NA)
#' @export
clean = function(x, which, ..., replace){
  UseMethod('clean')
}

#' @export
clean.numeric <- function(x, which = 'all', ..., replace=NULL){
  x = sensor(x)
  clean(x, which = which, ..., replace=replace)
}

#' @export
clean.data.frame <- function(x, which = 'all', ..., replace=NULL){
  x = sensor(x)
  clean(x, which = which, ..., replace=replace)
}

#' @export
clean.sensor <- function(x, which = 'all', ..., replace=NULL){
  
  if (which[1] == 'all'){
    if (!is.null(x$flags)){
      flag.i = sort(unique(unlist(sapply(flags(x), function(x) x$flag.i))))
      x$flags <- NULL
    } else {
      return(x)
    }
    
    if (is.null(replace)){
      x$sensor <- x$sensor[c(-flag.i),]
      rownames(x$sensor) <- seq_len(nrow(x$sensor))
    } else {
      x$sensor$x[flag.i] <- replace
    }
    return(x)
  } else if (is.numeric(which)) {
    if (!all(which %in% seq_len(length(x$flags))))
      stop('some or all of which=', paste(which,collapse=','), ' are not included in the flags')
    x$flags = x$flags[which] # drop the other flags
    clean(x, which = 'all', replace=replace)
  } else {
    x <- flag(x, flag.defs=which, ...)
    clean(x, which = 'all', replace=replace)
  }
}