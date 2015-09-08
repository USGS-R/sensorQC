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
  if ('windows' %in% names(formals(match.sqc.fun(expr))))
    flags <- window.sqc(expr, vals, windows)
  else
    flags <- value.sqc(expr, vals)
  
  return(flags & is.finite(flags) & !is.na(flags))
}

window.sqc <- function(expr, vals, windows){
  vals = append(set.vals(expr, vals), list(windows=windows))
  eval(expr, envir=vals)
}

value.sqc <- function(expr, vals){

  vals = set.vals(expr, vals)
  eval(expr, envir=vals)
}

set.vals <- function(expr, vals){
  vals <- do.call(paste0('to.',expr_var(expr)), list(vals=vals))
}

to.n <- function(vals){  
  tmp <- rle(vals)
  list('n'=rep(tmp$lengths,times = tmp$lengths))
}

to.x <- function(vals){
  list('x'=vals)
}
  
