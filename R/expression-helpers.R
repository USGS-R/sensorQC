
expr_fun <- function(expr){
  expr <- gsub("\\s","",expr)
  return(strsplit(expr,split = '[()]')[[1]][1])
}

expr_var <- function(expr){
  expr <- gsub("\\s","",expr)
  if (grepl(pattern = '[(]',expr)){
    var.nm <- strsplit(expr,split = '[()]')[[1]][2]
  } else {
    var.nm <- strsplit(expr,split = '[><=]')[[1]][1]
  }
  return(var.nm)
}

match.sqc.fun <- function(expr){
  fun = getAnywhere(expr_fun(expr))
  if (length(fun$objs) == 0)
    NULL
  else
    fun = fun$objs[[1]]
  return(fun)
}
