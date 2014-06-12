#'@title load in configuration file for sensorQC
#'@description 
#'Loads text file for sensorQC. Assumes *.yml file extension for deployment.  \cr
#'
#'@details a \code{sensorQC} function loading config.\cr 
#'
#'@param \code{deploy.name} name of config file (proceeding *.yml extension).
#'@param \code{folder} folder path for config file
#'@return a sqc object
#'@keywords sqc
#'@import yaml
#'@author
#'Jordan S. Read
#'@examples 
#'sqc <- load.sqc(deploy.name='pellerin',folder='../examples/')
#'@export

load.sqc <- function(deploy.name,folder='../examples/'){
  # need error handling
  sqc <- yaml.load_file(paste0(folder,deploy.name,'.yml'))
  num.outliers <- length(sqc$outlier_removal)
  for (i in 1:num.outliers){
    exp <- sqc$outlier_removal[[i]]$expression
    sqc$outlier_removal[[i]]$expression <- exp.replace(exp)
  }
  return(sqc)
}

exp.replace <- function(expression.in){
  if (grepl(pattern='==',x=expression.in)){
    return(expression.in)
  } else {
    expression.out <- sub(pattern='=', replacement='==', x=expression.in)
    expression.out <- sub(pattern='missing', replacement='is.na', x=expression.out)
    return(expression.out)
  }
  
}
