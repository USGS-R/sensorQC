clean.data <- function(){
  #is an example wrapper for sensorQC calls 
  
  sensor.data <- load.sensor(format='Pellerin')
  sensor.cnfg <- load.sqc(deploy.name='pellerin',folder='../examples/')
  
  windowed.data <- window.data(data.in=sensor.data)
  
  data.flags <- build.flags(data.in=windowed.data,sqc=sensor.cnfg)
  
  sensor.stats <- block.stats(windowed.data=windowed.data,data.flags=data.flags,rmv.cv=TRUE)
  
  old.sensor <- block.stats(windowed.data=windowed.data,rmv.cv=F)
  
  
  plot(old.sensor[, 1],rep(NA,nrow(old.sensor)),ylim=c(30,85),
       ylab="SUNA nitrate concentration (micromoles)",
       xlab="")
  
  points(windowed.data[, 1],windowed.data[, 2],col="green",pch=19,cex=0.4) # original high-res
  
  
  points(sensor.stats,col="green",pch=19,cex=0.4)
  
  lines(sensor.stats[, 1:2],lty=1,lwd=4)
  lines(old.sensor[,1:2],lty=6,col="red",lwd=2)

}

#clean.data()