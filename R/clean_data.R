#'@title cleans sensor data with user-specified routines
#'@description 
#'cleans sensor data according to details within the user-specified config (*yml) file  \cr
#'
#'@param deploy a string for the *.yml file name
#'@param folder a string which specifies the folder of the *.yml file
#'@param plot_diagnostic a boolean for creating a diagnostic plot
#'@param write_file a boolean for creating a timeseries file output
#'@return An optional plot and optional file output handle
#'@keywords methods
#'@author
#'Jordan S. Read
#'@examples 
#'folder <- system.file('extdata', package = 'sensorQC') 
#'clean_data(deploy = 'pellerin', folder = folder, plot_diagnostic=FALSE)
#'clean_data(deploy = 'pellerin', folder = folder, plot_diagnostic=TRUE, write_file=TRUE)
#'clean_data(deploy = 'no_window', folder = folder, plot_diagnostic=FALSE, write_file=FALSE)
#'@export
clean_data <- function(deploy,folder, plot_diagnostic=TRUE, write_file = FALSE){
  #is an example wrapper for sensorQC calls 
  
  #
  cnfg <- load_sqc(deploy.name=deploy,folder=folder)
  
  
  old_dir <- getwd()
  setwd(folder)
  sensor.file <- paste(cnfg$data_source[[1]]$folder_name, cnfg$data_source[[1]]$file_name,sep='')
  
  sensor.data <- load_sensor(file=sensor.file, format=cnfg$data_source[[1]]$format, date_type=cnfg$data_source[[1]]$date_type)
  setwd(old_dir)
  
  if ('window' %in% names(cnfg$data_source[[1]])){
    if (is.numeric(cnfg$data_source[[1]]$window)){
      windowed.data <- window_data(data.in=sensor.data, method = 'manual', window = cnfg$data_source[[1]]$window)
    } else {
      windowed.data <- window_data(data.in=sensor.data, method = 'auto')
    }
    inst.flags <- build_flags(data.in=windowed.data,sqc=cnfg$outlier_removal)
    
    sensor.stats <- block_stats(windowed.data = windowed.data, data.flags = inst.flags)
    
    block.flags <- build_flags(data.in=sensor.stats,sqc=cnfg$block_stats,verbose=TRUE,flatten=FALSE,compress=FALSE)
    
    simple.sqc <- list(outlier_removal=list(list(expression="x == 999999",type="error_code",description="logger error code"),
                                            list(expression='is.na(x)',type='error_code',description='missing data')))
    
    old.inst.flags <- build_flags(data.in=windowed.data,sqc=simple.sqc$outlier_removal,verbose=F)
    old.sensor <- block_stats(windowed.data=windowed.data,old.inst.flags)
    
    if (plot_diagnostic){
      plot_summary(inst.data=windowed.data,inst.flags,block.data=sensor.stats,block.flags,
                   compare.data=old.sensor,sqc=cnfg)
    }
    
    if (write_file){
      file_name <- paste0(deploy,"_sqc_out.tsv")
      output = file.path(folder,file_name)
      block_names <- get_block_flag_names(sqc = cnfg)
      flag_txt <- flag_out_squeeze(flag_names = block_names, flag_mat = block.flags)
      write.out <- cbind(sensor.stats[, c(1,2)], data.frame('.flags' = flag_txt)) # need drop method for this...
      
      write.table(write.out,file=output,col.names=TRUE, quote=FALSE, row.names=FALSE, sep="\t")
    }
    
  } else {
    
    inst.flags <- build_flags(data.in = sensor.data, sqc = cnfg$outlier_removal, compress = FALSE)
    if (plot_diagnostic){
      plot_summary(inst.data = sensor.data, inst.flags, sqc = cnfg)
    }
    
    if (write_file){
      file_name <- paste0(deploy,"_sqc_out.tsv")
      output = file.path(folder,file_name)
      flag_names <- get_inst_flag_names(sqc = cnfg)
      flag_txt <- flag_out_squeeze(flag_names = flag_names, flag_mat = inst.flags)
      write.out <- cbind(sensor.data[, c(1,2)], data.frame('.flags' = flag_txt)) # need drop method for this...
      
      write.table(write.out,file=output,col.names=TRUE, quote=FALSE, row.names=FALSE, sep="\t")
    }
    
  }
  

  
  
  
  

  

}
