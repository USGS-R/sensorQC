flag_wrap <- function(flag.type,data.in,expr,verbose=T){
  flags  <-  do.call(match.fun(flag.type),list(data.in=data.in,expr=expr)) 
  if (verbose){
    perc <- formatC(signif((sum(flags,na.rm = T)/length(flags))*100,digits=3), digits=3,format="fg", flag="#")
    verb.o <- paste0(flag.type,' ',expr,' created ',sum(flags,na.rm = T), ' flags (',perc,'%)\n')
    cat(verb.o)
  }
  
  return(flags)
}
#'@export
threshold <- function(data.in,expr='x > 99'){
  flags <- generic_sqc(vals = data.in$sensor.obs,expr)
  return(flags)
}

#'@export
error_code <- function(data.in,expr='x == -999'){
  vals <- list('x'=as.numeric(data.in$sensor.obs))
  flags <- generic_sqc(vals = vals, expr)
  return(flags)
}

#'@export
persistent <- function(data.in,expr='n > 10'){  
  tmp <- rle(data.in$sensor.obs)
  vals <- rep(tmp$lengths,times = tmp$lengths)
  flags <- generic_sqc(vals=vals,expr=expr)
  return(flags)
}
#'@export
stat_window <- function(data.in,expr){
  
  #var name?
  print(paste(names(data.in),collapse=' '))
  vals <- list(x=data.in)
  if (any(grepl(pattern = paste(names(data.in),collapse=' '),expr))){
    names(vals) <- get.expr.var(expr)
  }
  flags <- generic_sqc(vals,expr)
  return(flags)
}

generic_sqc <- function(vals,expr){

  test = tryCatch({
    test <- parse(text = expr)
  }, error = function(e) {
    stop(paste0('error evaluation expression ',expr))
  }, finally = {
    test
  })
  
  if (!is.list(vals)){
    vals <- list(x=vals)
    names(vals) <-get.expr.var(expr)
  }
  flags <- eval(test, envir=vals)
  
  return(flags)
}

get.expr.var <- function(expr){
  expr <- gsub("\\s","",expr)
  if (grepl(pattern = '[(]',expr)){
    var.nm <- strsplit(expr,split = '[()]')[[1]][2]
  } else {
    var.nm <- strsplit(expr,split = '[><=]')[[1]][1]
  }
  return(var.nm)
}
  
call.mad <- function(data.in){
  b = 1.4826    # assuming a normal distribution
  # from Huber 1981:
  med.val  <-	median(data.in)					# median of the input data
  abs.med.diff	<-	abs(data.in-med.val)	# absolute values minus med
  abs.med	<-	median(abs.med.diff)			# median of these values
  
  MAD  <-	b*abs.med
  
  MAD.normalized <- abs.med.diff/MAD # division by zero
  
  MAD.normalized[is.na(MAD.normalized)] = 0 # doesn't protect against NAs that enter in data.in
  return(MAD.normalized)
}
#'@title median absolute deviation outlier test
#'@name MAD
#'@aliases MAD
#'@aliases median.absolute.deviation
#'@param data.in a \code{sensorQC} data.frame.
#'@return a vector of MAD normalized values relative to an undefined rejection criteria (usually 2.5 or 3).
#'@keywords MAD
#'@author
#'Jordan S. Read
#'@export
MAD  <-  function(data.in){
  # does this method have to be public?	
  # what is the underlying distribution? (important for assigning "b")
  if (is.data.frame(data.in)){
    if (!"block.ID" %in% names(data.in)){stop("MAD can only accept numeric data, 
                                              or a data.frame with the block.ID column for windowed data")}
    MAD.out <- vector(length=nrow(data.in))
    un.win <- unique(data.in$block.ID)
    
    for (i in 1:length(un.win)){
      win.i <- un.win[i]
      val.i <- data.in$block.ID == win.i
      MAD.out[val.i] = call.mad(data.in$sensor.obs[val.i])
    }
    return(MAD.out)
  } else if (is.list(data.in)){
    data.in <- unlist(data.in)
    return(call.mad(data.in))
  } else {
    return(call.mad(data.in))
  }
  
}
call.cv <- function(data.in){
  CV <- 100*sd(data.in)/mean(data.in)
  CV <- rep(CV,length(data.in))
  return(CV)
}  
coefficient.of.variation <- function(data.in){
  
  
  if (is.data.frame(data.in)){
    if (!"block.ID" %in% names(data.in)){stop("CV can only accept numeric data, or a data.frame with the block.ID column for windowed data")}
    CV.out <- vector(length=nrow(data.in))
    un.win <- unique(data.in$block.ID)
    
    for (i in 1:length(un.win)){
      win.i <- un.win[i]
      val.i <- data.in$block.ID == win.i
      CV.out[val.i] = call.cv(data.in$sensor.obs[val.i])
    }
    return(CV.out)
  } else {
    return(call.cv(data.in))
  }
  
  
}

