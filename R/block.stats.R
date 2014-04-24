block.stats <- function(windowed.data,data.flags,params){
  
  clean.data <- exclude.outliers(data.in=windowed.data,data.flags,exclude.flags=c(1,2,3),write.log=FALSE)
  
  un.blcks <- unique(clean.data$block.ID)

  block.out <- init.sensor(length.out=length(un.blcks),append=c("CV"))
  

  for (i in seq_len(length(un.blcks))){
    use.i <- which(clean.data$block.ID==un.blcks[i])
    block.out$DateTime[i] <- mean(clean.data$DateTime[use.i])
    block.out$sensor.obs[i] <- mean(clean.data$sensor.obs[use.i])
    
  }
}