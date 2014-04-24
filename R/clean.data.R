clean.data <- function(){
  #is an example wrapper for sensorQC calls 
  
  sensor.data <- load.sensor(format='Pellerin')
  
  data.flags <- build.flags(data.in=sensor.data)
  
  windowed.data <- window.data(data.in=sensor.data) # this needs to only happen once (happens again in data.flags call)
  sensor.stats <- block.stats(windowed.data=windowed.data,data.flags=data.flags,rmv.cv=TRUE)
  
  old.sensor <- block.stats(windowed.data=windowed.data,data.flags=rep(0,length(data.flags)),rmv.cv=F)
  
  plot(sensor.stats[, 1],rep(NA,nrow(sensor.stats)),ylim=c(30,85),
       ylab="SUNA Nitrate (dims?)",
       xlab="")
  points(sensor.data,col="green",pch=19,cex=0.4)
  lines(old.sensor[,1:2],lty=6,col="red",lwd=2)
  lines(sensor.stats[, 1:2],lty=1,lwd=4)

}

clean.data()