#grouped row in a panelmap. This is always plotted first

panelet_group<-function(soln, soln.col,soln.name="group", border=FALSE, border.col="black", legend=FALSE, ...){

  if(any(is.na(soln))){stop("The group vector can't have NAs! Please remove NAs and provided an ordered vector")}
  dots <- list(...)
  n<-length(soln)
  group.col = get.colvector(soln,soln.col)
  barplot(rep(1,n),col=group.col$labels.col,border=group.col$labels.col,axes=F,space=0)
  mtext(soln.name,side=2,las=2,...)
   if(border==TRUE){
     dots2 = dots; dots2$col=border.col; dots2$x = c(0,n); dots2$y=c(0,0)
     do.call(lines, args=dots2)

     dots2 = dots; dots2$col=border.col; dots2$x = c(0,n); dots2$y=c(1,1)
     do.call(lines, args=dots2)

     dots2 = dots; dots2$col=border.col; dots2$v=0
     do.call(abline, args = dots2)

     dots2 = dots; dots2$v=n; dots2$col=border.col
     do.call(abline, args = dots2)

  }

  key = group.col$key
  if(legend==TRUE){ key[1,] = paste0(soln.name, "=",key[1,])}
  return(list(key=key))
}
