clean.data <- function(deploy='pellerin'){
  #is an example wrapper for sensorQC calls 
  
  #
  cnfg <- load.sqc(deploy.name=deploy,folder='../examples/')
  
  sensor.file <- paste(cnfg$data_source[[1]]$folder_name,cnfg$data_source[[1]]$file_name,sep='')
  
  sensor.data <- load.sensor(filename=sensor.file, format=cnfg$data_source[[1]]$format, date.type=cnfg$data_source[[1]]$date_type)
  
  windowed.data <- window_data(data.in=sensor.data)
  
  inst.flags <- build_flags(data.in=windowed.data,sqc=cnfg$outlier_removal)

  sensor.stats <- block_stats(windowed.data=windowed.data,data.flags=inst.flags)
  
  block.flags <- build_flags(data.in=sensor.stats,sqc=cnfg$block_stats,verbose=TRUE,flatten=FALSE,compress=FALSE)
  
  simple.sqc <- list(outlier_removal=list(list(expression="x == 999999",type="error_code",description="logger error code"),
                                                        list(expression='is.na(x)',type='error_code',description='missing data')))
  
  old.inst.flags <- build_flags(data.in=windowed.data,sqc=simple.sqc$outlier_removal,verbose=F)
  old.sensor <- block_stats(windowed.data=windowed.data,old.inst.flags)
  
  plot_summary(inst.data=windowed.data,inst.flags,block.data=sensor.stats,block.flags,compare.data=old.sensor,sqc=cnfg)

  

}


#