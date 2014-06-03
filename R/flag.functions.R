

threshold <- function(data.in,expr='x > 99'){
  test <- parse(text = expr)
  data.list <- list(x=data.in$sensor.obs)
  names(data.list) <- substr(expr,1,1)
  eval(test, envir=data.list)
}