calc_flags <- function(x, ...){
  UseMethod('calc_flags')
}

#' @export
calc_flags.sensor <- function(sensor, expr, which.flagged=TRUE){
  flags  <-  sqc(expr=expr, vals=values(sensor), windows=windows(sensor))
  
  if (which.flagged)
    return(which(flags))
  else  
    return(flags)
}


sqc <- function(expr, vals, windows, ...){

  expr = tryCatch({
    parse(text = expr)
  }, error = function(e) {
    stop(paste0('error evaluation expression ',expr))
  })

  vals = set.args(expr, vals, windows)
  
  flags <- eval(expr, envir=vals)
  
  return(flags & is.finite(flags) & !is.na(flags))
}

set.args <- function(expr, vals, windows){
  val.call <- function(x){
    do.call(paste0('to.',x), list(vals=vals, windows=windows))
  }
  arg.names = expr_var(expr)
  args = sapply(arg.names, val.call)
  return(setNames(args, arg.names))
}

to.n <- function(vals, ...){  
  tmp <- rle(vals)
  list('n'=rep(tmp$lengths,times = tmp$lengths))
}

to.x <- function(vals, ...){
  list('x'=vals)
}
  
to.w <- function(..., windows){
  list('w'=windows)
}
