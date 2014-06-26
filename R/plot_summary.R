#'@export
plot_summary <- function(inst.data,inst.flags,block.data,block.flags,new.plot=TRUE){
  panels = c(2,2,2,1,1,1,1,1,1,1,1,1,3)
  
  
  xlm = c(min(inst.data[, 1]), max(inst.data[, 1]))
  plot(inst.data[, 1],rep(NA,nrow(inst.data)),ylim=c(30,85),
       ylab="SUNA nitrate concentration (micromoles)",
       xlab="",xlim=xlm)
  
  layout(panels)
  par(mai=c(0,.25,0, .1),omi=c(0,0,0,0))
  plot(inst.data[, 1],rep(NA,nrow(inst.data)),ylim=c(30,85),
       ylab="SUNA nitrate concentration (micromoles)",
       xlab="")
  points(inst.data[, 1:2],col="green",pch=19,cex=0.4)
  points(block.data[!block.flags, 1:2],lty=1,lwd=4,pch=19,cex=0.8)
  plot(c(1,NA),c(NA,0),ylim = c(0,ncol(inst.flags)),xlim = xlm,xlab=NA,axes=F)
  #axis(1,at=seq(0, 200, 10),las=1, cex.axis=cex.box, tck=tck,labels=NA,lwd=2)
  for (i in 1:ncol(inst.flags)){
    flags <- inst.flags[, i]
    use.i <- flags[!is.na(flags)]
    points(inst.data[use.i,1],rep(i,length(use.i)),pch=6)
  }
  
}