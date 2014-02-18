get.outliers	<- 	function(data.in,method='median.absolute.deviation',reject.criteria=3,na.rm=FALSE){
	# returns boolean array, equal in size to data.in, with outlier=T or F
	# NAs are treated as outliers ONLY if na.rm=TRUE
	

	#---- <test input data> ----
	if (!is.numeric(data.in) & !all(is.na(data.in))){
		stop("input data must be of type 'numeric'")
	}
	#---- </test input data> ----
	
	#---- <remove NA if appropriate> ----
	if (na.rm){
		na.idx	<-	is.na(data.in)
		outlier.na.rm	<-	vector(length=length(data.in))
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