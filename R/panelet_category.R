#plot panelet containg two or more categories

panelet_category<-function(pp,pp.col,soln, var.n="var.n",labels.col="black",NA.flag=FALSE, NA.col="grey", get.pval=FALSE, border=FALSE, cex=1, border.col="black", legend=FALSE){

  if(length(pp) != length(soln)){ stop("length of pp i snot equal to soln vector")}

  n<-length(soln)
  pcol<-get.colvector(pp, pp.col[1:length(unique(na.omit(pp)))], NA.flag, NA.col)
  barplot(rep(1,n),col=pcol$labels.col,border=pcol$labels.col,axes=F,space=0)

  mtext(var.n,side=2,las=2,cex=cex, col=labels.col)
  if(get.pval==TRUE){
    tab=get.summary2(soln,pp,1, var.n=var.n)
    pval= tab[1,"pval"]
    mtext(paste0("P=",pval),side=4,las=2,cex=cex, col=labels.col)
  }

  if(border==TRUE){
    abline(v=0, lwd=1.5, col="black")
    at.points = cumsum(table(soln))
    for(i in 1:length(at.points)){ abline(v=at.points[i], lwd=1.5, col=border.col)}
  }
  #option to suprress table?
  #should we apply multiple adjustment
  key = pcol$key
  if(legend==TRUE){ key[1,] = paste0(var.n, "=",key[1,]) }
  if(get.pval==TRUE){return(list(key=key,tab=tab))}
  return(list(key=key))
}
