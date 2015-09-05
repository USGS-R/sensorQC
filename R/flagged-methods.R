

flags <-function(x){
  UseMethod('flags')
}

#' @export
flags.flagged <- function(flagged){
  flagged$flags
}
#' @export
`[.flagged` <- function(x, i, j, drop){
  x[i]
}