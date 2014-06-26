clean.data <- function(sensor.file='../examples/test_data.txt',fl.format='Pellerin',deploy='pellerin'){
  #is an example wrapper for sensorQC calls 
  
  sensor.data <- load.sensor(filename=sensor.file, format=fl.format)
  cnfg <- load.sqc(deploy.name=deploy,folder='../examples/')
  
  windowed.data <- window_data(data.in=sensor.data)
  
  inst.flags <- build_flags(data.in=windowed.data,sqc=cnfg$outlier_removal)

  sensor.stats <- block_stats(windowed.data=windowed.data,data.flags=inst.flags)
  
  block.flags <- build_flags(data.in=sensor.stats,sqc=cnfg$block_stats,verbose=TRUE,flatten=TRUE,compress=FALSE)
  
  simple.sqc <- list(outlier_removal=list(list(expression="x == 999999",type="error_code",description="logger error code"),
                                                        list(expression='is.na(x)',type='error_code',description='missing data')))
  
  old.inst.flags <- build_flags(data.in=windowed.data,sqc=simple.sqc$outlier_removal,verbose=F)
  old.sensor <- block_stats(windowed.data=windowed.data,old.inst.flags)
  
  plot_summary(inst.data=windowed.data,inst.flags,block.data=sensor.stats,block.flags)

  lines(old.sensor[,1:2],lty=6,col="red",lwd=2)

}

#clean.data()