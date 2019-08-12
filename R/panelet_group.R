#grouped row in a panelmap. This is always plotted first

panelet_group<-function(soln, soln.col,soln.name="group",cex=1, border=FALSE, legend=FALSE){

  if(any(is.na(soln))){stop("The group vector can't have NAs! Please remove NAs and provided an ordered vector")}
  n<-length(soln)
  group.col = get.colvector(soln,soln.col)
  barplot(rep(1,n),col=group.col$labels.col,border=group.col$labels.col,axes=F,space=0)
  mtext(soln.name,side=2,las=2,cex=cex)
  if(border==TRUE){lines(x=c(0,n),y=c(0,0)); lines(x=c(0,n),y=c(1,1));abline(v=c(0,n))}
  key = group.col$key
  if(legend==TRUE){ key[1,] = paste0(soln.name, "=",key[1,])}
  return(list(key=key))
}
