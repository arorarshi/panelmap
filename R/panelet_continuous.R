#plot panelet containing continuous data

panelet_continuous<-function(pp,pp.col,soln, var.n="var.n",NA.flag=TRUE, NA.col="grey", get.pval=FALSE, border=FALSE,border.col="black", ...)
{

  if(any(is.na(pp)) & NA.flag==FALSE){ stop("There are NAs in data. Turn NA.flag=TRUE and specify NA.col. NA.col should be different than gradient of pp.col") }

  if(length(pp) != length(soln)){ stop("length of pp is not equal to soln vector")}
  dots <- list(...)
  ii <- cut(pp, breaks = seq(min(pp,na.rm=T), max(pp,na.rm=T), length.out = 100), include.lowest = TRUE)

  ## Use bin indices, ii, to select color from vector of n-1 equally spaced colors
  pcol <- colorRampPalette(pp.col)(99)[ii]
  n<-length(soln)
  if(NA.flag==TRUE){ pcol[which(is.na(pcol))] = NA.col}
  barplot(rep(1,n),col=pcol,border=pcol,axes=F,space=0)
  mtext(var.n,side=2,las=2,...)

  if(get.pval==TRUE){
    tab=get.summary2(soln,pp,2, var.n=var.n)
    pval= tab[1,"pval"]
    mtext(paste0("P=",pval),side=4,las=2,...)
  }

  if(border==TRUE){
    dots2 = dots; dots2$v=0; dots2$col=border.col
    do.call(abline, args = dots2)
    at.points = cumsum(table(soln))
    for(i in 1:length(at.points)){
      dots2 = dots; dots2$v=at.points[i]; dots2$col=border.col
      do.call(abline, args = dots2)}
  }

  if(get.pval==TRUE){return(list(key=NULL, tab=tab))}
  return(list(key=NULL))

}
