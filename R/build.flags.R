build.flags <- function(data.in,params){
  
  # creates flag array based in data.in and parameters
  
  data.flags <- vector(length=nrow(data.in),mode="numeric") # vector of zeros
  # types of flagged data: NA=1, 999999=2, MAD=3, etc...
  # need a matching code function. Or summary function which parameterizes this
  
  
  # NA check
  rplc.val = 1
  data.flags[is.na(data.in$sensor.obs)] <- rplc.val
  
  # loggernet check
  rplc.val = 2
  data.flags[data.in$sensor.obs==999999] <- rplc.val
  
  # window data
  windowed.data <- window.data(data.in=data.in,method="auto")
  
  # MAD (if applicable) check
  rplc.val = 3
  un.blcks <- unique(windowed.data$block.ID)
  for (i in seq_len(length(un.blcks))){
    use.i <- which(windowed.data$block.ID==un.blcks[i] & data.flags==0) # ignore values that are already flagged
    check.dat <- windowed.data$sensor.obs[use.i]
    mad.flag <- get.outliers(check.dat,method='median.absolute.deviation',reject.criteria=2.5,na.rm=FALSE)
    data.flags[use.i[mad.flag]] <- rplc.val
  }
  
  # call window.data, then get.outliers, etc. *USE params*
  
  
  return(data.flags)
  
}