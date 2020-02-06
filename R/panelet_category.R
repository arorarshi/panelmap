#plot panelet containg two or more categories

panelet_category<-function(pp,pp.col,soln, var.n="var.n",NA.flag=FALSE, NA.col="grey", get.pval=FALSE, border=FALSE, border.col="black", legend=FALSE, ...){

  if(length(pp) != length(soln)){ stop("length of pp i snot equal to soln vector")}
  dots <- list(...)
  n<-length(soln)
  pcol<-get.colvector(pp, pp.col, NA.flag, NA.col)
  barplot(rep(1,n),col=pcol$labels.col,border=pcol$labels.col,axes=F,space=0)

  mtext(var.n,side=2,las=2,...)
  if(get.pval==TRUE){
    tab=get.summary2(soln,pp,1, var.n=var.n)
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

  #option to suprress table?
  #should we apply multiple adjustment
  key = pcol$key
  if(legend==TRUE){ key[1,] = paste0(var.n, "=",key[1,]) }
  if(get.pval==TRUE){return(list(key=key,tab=tab))}
  return(list(key=key))
}
