#'@title load in configuration file for sensorQC
#'@description 
#'Loads text file for sensorQC. Assumes *.sqc file extension for deployment.  \cr
#'
#'@details a \code{sensorQC} function loading config.\cr 
#'
#'@param \code{deploy.name} name of config file (proceeding *.sqc extension).
#'@param \code{folder} folder path for config file
#'@return a sqc object
#'@keywords sqc
#'@author
#'Jordan S. Read
#'@examples 
#'sqc <- load.sqc(deploy.name='pellerin',folder='../examples/')
#'@export

load.sqc <- function(deploy.name,folder='../examples/'){
  # need error handling
  sqc <- read.table(paste0(folder,deploy.name,'.sqc'),sep='\t',header=T,colClasses=c("character","character","character"))
  sqc$expression <- exp.replace(sqc$expression)
  return(sqc)
}

exp.replace <- function(expression.in){
  
  expression.out <- sub(pattern='=', replacement='==', x=expression.in)
  return(expression.out)
}