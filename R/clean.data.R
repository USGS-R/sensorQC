clean.data <- function(sensor.file='../examples/test_data.txt',fl.format='Pellerin',deploy='pellerin'){
  #is an example wrapper for sensorQC calls 
  
  sensor.data <- load.sensor(filename=sensor.file, format=fl.format)
  cnfg <- load.sqc(deploy.name=deploy,folder='../examples/')
  
  windowed.data <- window_data(data.in=sensor.data)
  
  data.flags <- build_flags(data.in=windowed.data,sqc=cnfg)
  
  sensor.stats <- block_stats(windowed.data=windowed.data,data.flags=data.flags,rmv.cv=FALSE) # was true
  
  simple.sqc <- list(outlier_removal=list(list(expression="x == 999999",type="error_code",description="logger error code"),
                                                        list(expression='is.na(x)',type='error_code',description='missing data')))
  data.flags <- build_flags(data.in=windowed.data,sqc=simple.sqc
                            ,verbose=F)
  old.sensor <- block_stats(windowed.data=windowed.data,data.flags,rmv.cv=F)
  
  
  plot(old.sensor[, 1],rep(NA,nrow(old.sensor)),ylim=c(0,15),
       ylab="SUNA nitrate concentration (micromoles)",
       xlab="")
  
  points(windowed.data[, 1],windowed.data[, 2],col="green",pch=19,cex=0.4) # original high-res
  
  
  points(sensor.stats,col="black",pch=19,cex=0.4)
  
  points(sensor.stats[, 1:2],lty=1,lwd=4,pch=19,cex=0.8)
  lines(old.sensor[,1:2],lty=6,col="red",lwd=2)

}

#clean.data()