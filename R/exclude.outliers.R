exclude.outliers <- function(data.in,data.flags,exclude.flags,write.log=TRUE){
  
  # takes in data and data flags, gets rid of outliers, writes log file if applicable
  
  if (nrow(data.in) != length(data.flags)){
    stop("number of columns for data and flags must be equal")
  }
  
  rmv.i <- data.flags %in% exclude.flags 
  
  data.out <- data.in[!rmv.i, ]
  
  if (write.log){
    # call write log here..
  }
  
  return(data.out)
}