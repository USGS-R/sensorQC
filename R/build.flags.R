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
  
  # MAD (if applicable) check
  rplc.val = 3
  # call window.data, then get.outliers, etc. *USE params*
  
  
  return(data.flags)
  
}