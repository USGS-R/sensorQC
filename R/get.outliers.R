get.outliers	<- 	function(data.in,method='median.absolute.deviation',reject.criteria=3){
	# returns boolean array, equal in size to data.in, with outlier=T or F
	# NAs are treated as outliers
	
	
	# what is the underlying distribution? (important for assigning "b")
	
	#---- <test input data> ----
	if (!is.numeric(data.in)){
		stop("input data must be of type 'numeric'")
	}
	#---- </test input data> ----
	
	
	#---- <call outlier detection> ----
	if (method=='median.absolute.deviation'){
		outlier.indices	<-	outliersMAD(data.in,reject.criteria=reject.criteria)
		
	} else {
		stop(paste('method',method,'not supported by this function',sep=' '))
	}
	#---- </call outlier detection> ----
	
	
	return(outlier.indices)
}

outliersMAD	<-	function(data.in,reject.criteria){
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