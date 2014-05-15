#'@title gets outlier values
#'@description 
#'gets outlier values according to the method chosen.  \cr
#'
#'@usage
#'outliers <- get.outliers(data.in,method='median.absolute.deviation',reject.criteria=3,na.rm=FALSE)
#'
#'@param \code{data.in} a time series data.frame
#'@param \code{method} a string which matches a valid outlier detection method
#'@param \code{reject.criteria} a numeric value to determine the stringency of the outlier detection criteria. Miller (1991) proposes the values of 3 (very conservative), 2.5 (moderately conserva- tive) or even 2 (poorly conservative)
#'@param \code{na.rm} boolean to determine if NAs should be removed before analysis. If TRUE, NAs will be outlier=TRUE.
#'@return A data.frame with DateTime and values
#'@keywords outliers
#'@author
#'Jordan S. Read
#'@examples 
#'dates <- seq(as.POSIXct('1999-01-01'),by=1,length.out=12)
#'values <- runif(12,2,4)
#'data.in <- data.frame("DateTime"=dates,"sensor.obs"=values)
#'data.flags <- c(0,0,0,0,4,0,3,0,3,0,0,0)
#'exclude.flags <- c(1,4) # only exclude type 1 and type 4 flags
#'
#'exclude.outliers(data.in,data.flags,exclude.flags,write.log=F)
#'@export

get.outliers	<- 	function(data.in,method='median.absolute.deviation',reject.criteria=3,na.rm=FALSE){
	
  # returns boolean array, equal in size to data.in, with outlier=T or F
	# NAs are treated as outliers ONLY if na.rm=TRUE
	

	#---- <test input data> ----
	if (!is.numeric(data.in) & all(is.na(data.in))){
		stop("input data must be of type 'numeric'")
	}
	#---- </test input data> ----
	
	#currently does not support multi-dimensional arrays
	if (!is.null(dim(data.in))){
		stop('function only currently supports 1D vectors')
	}
	
	#---- <remove NA if appropriate> ----
	if (na.rm){
		na.idx	<-	is.na(data.in)
		outlier.na.rm	<-	as.logical(data.in*FALSE)
		outlier.na.rm[na.idx]	<-	TRUE
		data.in	<-	data.in[!na.idx] # need to handle zero length
	}
	#---- </remove NA if appropriate> ----
	
	#---- <call outlier detection> ----
	# has error handling if method is not supported
	outlier.indices	<-	do.call(match.fun(method),list(data.in=data.in,reject.criteria=reject.criteria))
	#---- </call outlier detection> ----

	
	if (na.rm){
		outlier.na.rm[!na.idx] <- outlier.indices	# assumes same order. The rest should be TRUE
		outlier.indices	<-	outlier.na.rm
	}
	
	return(outlier.indices)
}

median.absolute.deviation	<-	function(data.in,reject.criteria){
  # does this method have to be public?	
	# what is the underlying distribution? (important for assigning "b")
	
	b = 1.4826		# assuming a normal distribution
	# from Huber 1981:
	med.val	<-	median(data.in)					# median of the input data
	abs.med.diff	<-	abs(data.in-med.val)	# absolute values minus med
	abs.med	<-	median(abs.med.diff)			# median of these values

	
	MAD	<-	b*abs.med
	
	reject.high	<-	med.val+reject.criteria*MAD
	reject.low	<-	med.val-reject.criteria*MAD
	outlier.indices	<-	(data.in>reject.high | data.in<reject.low)
	return(outlier.indices)
}