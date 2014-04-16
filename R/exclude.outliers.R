exclude.outliers <- function(data.in,flags,write.log=TRUE){
  
  # takes in data and data flags, gets rid of outliers, writes log file if applicable
  
  if (nrow(data.in) != length(flags)){
    stop("number of columns for data and flags must be equal")
  }
  
  
  return(data.out)
}