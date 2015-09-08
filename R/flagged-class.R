
flagged <- function(x, ...){
  UseMethod('flagged')
}

flagged.sensor <- function(sensor, flag.defs, ...){
  
  add_indices <- function(x) {
    for (i in seq_len(length(x))){
      x[[i]]=append(x[[i]],list('flag.i'=c()))
    }
    x
  }
  flagged <- list('sensor'=sensor, 
                  flags = lapply(flagged(flag.defs, ...), add_indices))
  class(flagged) <- 'flagged'
  return(flagged)
}

flagged.data.frame <- function(x, ...){
  UseMethod('flagged',sensor(x))
}

#' @export
flagged.character <- function(x, ...){
  flag.defs = append(list(x), list(...))
  list('inst'=lapply(flag.defs, function(x) list('expression'=x)))
}
#' @export
flagged.list <- function(x, ...){
  x
}

#' @export
flagged.qconfig <- function(qconfig){
  list('inst'=qconfig[['outlier_removal']],
       'window'=qconfig[['block_stats']])
}
