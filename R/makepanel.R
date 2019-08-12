
#wrapper function - plotting an entire panelmap

makepanel<-function(soln, soln.name,soln.col,mat, mat.col,mat.type, legend=FALSE, get.pval=FALSE,NA.flag=FALSE, NA.col="grey", border=FALSE, cex=1){

  cnames = c(colnames(mat), soln.name)
  legend.list = list()

  plot.width = max(mapply(function(x) nchar(x), cnames)) + 2

  plot.height = ncol(mat) + 2
  if(legend==TRUE){ plot.height = plot.height + 2}

  par(mfrow=c(plot.height,1),mar=c(0,plot.width,0,12))

  tt=panelet_group(soln, soln.col,soln.name, legend=legend, border=border, cex=cex)
  legend.list[[1]] = tt$key
  fmat=NULL

  for(i in 1:ncol(mat)){

    if(mat.type[i] ==1){
      tt=panelet_category(mat[,i], mat.col[[colnames(mat)[i]]], soln, colnames(mat)[i], get.pval=get.pval, NA.flag=NA.flag, NA.col=NA.col, border=border, cex=cex, legend=legend)}

    if(mat.type[i] ==2){ tt=panelet_continuous(mat[,i], mat.col[[colnames(mat)[i]]], soln, colnames(mat)[i], get.pval=get.pval, NA.flag=NA.flag, NA.col=NA.col, border=border, cex=cex)    }

    if( i == ncol(mat) & border==TRUE ){ lines(x=c(0,length(soln)),y=c(0,0)) }

    if(!(is.null(tt$key))) {legend.list[[i+1]] = tt$key}

    if(get.pval==TRUE){fmat = rbind(fmat, tt$tab)}
  }

  if(legend==TRUE){ boxplot(rep(1,length(soln)),col="white", border="white",axes=F, space=0 )
    legend.vec= unlist(lapply(legend.list, function(x) x[1,]))
    legend.col = unlist(lapply(legend.list, function(x) x[2,]))
    legend("topright",legend=legend.vec, fill=legend.col, bty="n", ncol=5)
  }
  return(fmat)
}

