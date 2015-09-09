
expr_fun <- function(expr){
  expr <- gsub("\\s","",expr)
  return(strsplit(expr,split = '[()]')[[1]][1])
}

expr_var <- function(expr){
  expr <- gsub("\\s","",expr)
  if (grepl(pattern = '[(]',expr)){
    return(split_vars(strsplit(expr,split = '[()]')[[1]][2]))
  } else {
    return(strsplit(expr,split = '[><=]')[[1]][1])
  }
}
split_vars <- function(x){
  strsplit(x, split = '[,]')[[1]]
}

match.sqc.fun <- function(expr){
  fun = getAnywhere(expr_fun(expr))
  if (length(fun$objs) == 0)
    NULL
  else
    fun = fun$objs[[1]]
  return(fun)
}
