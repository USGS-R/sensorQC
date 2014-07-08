#'@title load in configuration file for sensorQC
#'@description 
#'Loads text file for sensorQC. Assumes *.yml file extension for deployment.  \cr
#'
#'@details a \code{sensorQC} function loading config.\cr 
#'
#'@param deploy.name name of config file (proceeding *.yml extension).
#'@param folder folder path for config file
#'@return a sqc object
#'@keywords methods
#'@import yaml
#'@author
#'Jordan S. Read
#'@examples 
#'sqc <- load.sqc(deploy.name='pellerin',folder='../examples/')
#'@export

load.sqc <- function(deploy.name,folder='../examples/'){
  # need error handling
  # yaml warns for "incomplete final line". Suppressing this.
  sqc <- suppressWarnings(yaml.load_file(paste0(folder,deploy.name,'.yml')))
  num.types <- length(sqc)
  for (k in 1:num.types){
    num.subs <- length(sqc[[k]])
    for (i in 1:num.subs){
      exp <- sqc[[k]][[i]]$expression
      if (!is.null(exp)){
        repl.lst <- exp.replace(exp)
        sqc[[k]][[i]]$expression <- repl.lst$expression
        sqc[[k]][[i]]$alias <- repl.lst$alias
      }
      date.form <- sqc[[k]][[i]]$date_type
      if (!is.null(date.form)){
        sqc[[k]][[i]]$date_type <- date.replace(date.form)
      }
    }
  }
  return(sqc)
}

exp.replace <- function(expression.in){
  alias <- expression.in
  expression.out <- expression.in
  
  # test for percent-based text
  expression.out <- sub(pattern='%', replacement='', x=expression.out)
  expression.out <- sub(pattern='missing', replacement='is.na', x=expression.out)
  
  if (grepl(pattern='==',x=expression.out)){
    return(list(expression=expression.out,alias=alias))
  } else {
    expression.out <- sub(pattern='=', replacement='==', x=expression.out)
    return(list(expression=expression.out,alias=alias))
  }
  
}

date.replace <- function(date.form){
  alias.date <- list("YYYY-mm-dd HH:MM" = "%Y-%m-%d %H:%M",
                     "mm/dd/YYYY HH:MM" = "%m/%d/%Y %H:%M")
  
  if (!date.form %in% names(alias.date)){
    stop(paste('date format ',date.form,' not supported'))
  }
  u.i <- which(grepl(date.form, names(alias.date)))
  return(alias.date[[u.i]])
}
