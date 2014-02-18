get.outliers	<- 	function(data.in,method='median.absolute.deviation'){
	# returns boolean array, equal in size to data.in, with outlier=T or F
	# NAs are treated as outliers
	
	# what is the underlying distribution?
	
	#---- <test input data> ----
	if (!is.numeric(data.in)){
		stop("input data must be of type 'numeric'")
	}
	#---- </test input data> ----
	
	
	#---- <call outlier detection> ----
	if (method=='median.absolute.deviation'){
		# from Huber 1981:
		b = 1.4826
		MAD	<-	b*median(data.in)*()
	} else {
		stop(paste('method',method,'not supported by this function',sep=' '))
	}

	
	
	return(outlier.indices)
}