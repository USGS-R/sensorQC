#'@title cleans sensor data with user-specified routines
#'@description 
#'cleans sensor data according to details within the user-specified config (*yml) file  \cr
#'
#'@param deploy a string for the *.yml file name
#'@param folder a string which specifies the folder of the *.yml file
#'@param plot.diagnostic a boolean for creating a diagnostic plot
#'@param write.file a boolean for creating a timeseries file output
#'@return An optional plot and optional file output handle
#'@keywords methods
#'@author
#'Jordan S. Read
#'@examples 
#'\dontrun{
#'clean.data(deploy='pellerin',folder='/[User]/Desktop/Science Projects/sensorQC/',plot.diagnostic=FALSE)
#'clean.data(deploy='pellerin',folder='/[User]/Desktop/Science Projects/sensorQC/',plot.diagnostic=TRUE,write.file=TRUE)
#'}
#'@export
clean.data <- function(deploy='pellerin',folder='../examples/',plot.diagnostic=TRUE, write.file= FALSE){
  #is an example wrapper for sensorQC calls 
  
  #
  cnfg <- load.sqc(deploy.name=deploy,folder=folder)
  
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
  
  if (plot.diagnostic){
    plot_summary(inst.data=windowed.data,inst.flags,block.data=sensor.stats,block.flags,
                 compare.data=old.sensor,sqc=cnfg)
  }
  
  if (write.file){
    output = paste0(folder,deploy,"_sqc_out.tsv")
    flat.block <- flatten_flags(block.flags)
    write.out <- sensor.stats[!flat.block, ]
    write.table(write.out,file=output,col.names=TRUE, quote=FALSE, row.names=FALSE, sep="\t")
  }
  
  

  

}
