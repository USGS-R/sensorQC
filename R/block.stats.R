#'@export
block_stats <- function(windowed.data,data.flags=NULL,rmv.cv=TRUE){
  
  if (!is.null(data.flags)){
    un.flags <- unique_flags(data.flags)
    grab.idx <- seq_len(nrow(windowed.data))
    skp.i <- grab.idx %in% un.flags
    clean.data <- windowed.data[!skp.i, ]
  } else {
    clean.data <- windowed.data
  }
  
  
  un.blcks <- unique(clean.data$block.ID)

  block.out <- init.sensor(length.out=length(un.blcks),append=c("CV"))
  

  for (i in seq_len(length(un.blcks))){
    use.i <- which(clean.data$block.ID==un.blcks[i])
    block.out$DateTime[i] <- mean(clean.data$DateTime[use.i])
    block.out$sensor.obs[i] <- mean(clean.data$sensor.obs[use.i])
    block.out$CV[i] <- call.cv(clean.data$sensor.obs[use.i])[1]
  }
  
  if (rmv.cv){

    block.rmv <- call.mad(block.out$CV) > 3
    block.out = block.out[!block.rmv, ]
  }
  
  
  return(block.out)
}

