block.stats <- function(windowed.data,data.flags,params){
  
  clean.data <- exclude.outliers(data.in=windowed.data,data.flags,exclude.flags=c(1,2,3),write.log=FALSE)
  
  un.blcks <- unique(clean.data$block.ID)

  block.out <- init.sensor(length.out=length(un.blcks))

  for (i in seq_len(length(un.blcks))){
    
  }
}