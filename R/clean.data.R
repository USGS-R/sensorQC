clean.data <- function(sensor.file='../examples/test_data.txt'){
  #is an example wrapper for sensorQC calls 
  
  sensor.data <- load.sensor(filename=sensor.file, format='Pellerin')
  cnfg <- load.sqc(deploy.name='pellerin',folder='../examples/')
  
  windowed.data <- window.data(data.in=sensor.data)
  
  data.flags <- build.flags(data.in=windowed.data,sqc=cnfg$outlier_removal)
  
  sensor.stats <- block.stats(windowed.data=windowed.data,data.flags=data.flags,rmv.cv=TRUE)
  
  simple.sqc <- list(outlier_removal=list(expression="x == 999999",type="error_code",description="logger error code"))
  data.flags <- build.flags(data.in=windowed.data,sqc=simple.sqc
                            ,verbose=F)
  old.sensor <- block.stats(windowed.data=windowed.data,data.flags,rmv.cv=F)
  
  
  plot(old.sensor[, 1],rep(NA,nrow(old.sensor)),ylim=c(30,85),
       ylab="SUNA nitrate concentration (micromoles)",
       xlab="")
  
  points(windowed.data[, 1],windowed.data[, 2],col="green",pch=19,cex=0.4) # original high-res
  
  
  points(sensor.stats,col="black",pch=19,cex=0.4)
  
  points(sensor.stats[, 1:2],lty=1,lwd=4,pch=19,cex=0.8)
  lines(old.sensor[,1:2],lty=6,col="red",lwd=2)

}

#clean.data()