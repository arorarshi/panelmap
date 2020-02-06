
#wrapper function - plotting an entire panelmap

makepanel<-function(soln, soln.name,soln.col,mat, mat.col,mat.type, legend=FALSE, get.pval=FALSE,NA.flag=FALSE, NA.col="grey", border=FALSE, border.col="black", lnrow=3, lncol=5,lcex=1, legend.vec=NULL, legend.col=NULL, lheight=1, ...){

  cnames = c(colnames(mat), soln.name)
  legend.list = list()
  dots<-list(...)

  plot.width = max(mapply(function(x) nchar(x), cnames)) + 2
  plot.height = ncol(mat) + 2
  if(legend){
    if(lheight > 1){plot.height = plot.height + lheight+1}
    else if(lheight == 1) {plot.height = plot.height + 2}
  }

  par(mfrow=c(plot.height,1),mar=c(0,plot.width,0,12))

  tt=panelet_group(soln, soln.col,soln.name, legend=legend, border=border, border.col=border.col, ...)
  legend.list[[1]] = tt$key
  fmat=NULL

  for(i in 1:ncol(mat)){

    if(mat.type[i] ==1){
      tt=panelet_category(mat[,i], mat.col[[colnames(mat)[i]]], soln, colnames(mat)[i], get.pval=get.pval, NA.flag=NA.flag, NA.col=NA.col, border=border, border.col=border.col, legend=legend, ...)}

    if(mat.type[i] ==2){ tt=panelet_continuous(mat[,i], mat.col[[colnames(mat)[i]]], soln, colnames(mat)[i], get.pval=get.pval, NA.flag=NA.flag, NA.col=NA.col, border=border, border.col=border.col, ...)    }

    if( i == ncol(mat) & border==TRUE ){ dots2 = dots; dots2$col=border.col; dots2$x = c(0,length(soln)); dots2$y=c(0,0)
    do.call(lines, args=dots2)}

    if(!(is.null(tt$key))) {legend.list[[i+1]] = tt$key}

    if(get.pval==TRUE){fmat = rbind(fmat, tt$tab)}
  }

  if(legend==TRUE){

    if(is.null(legend.vec) & is.null(legend.col)){
      legend.vec= unlist(lapply(legend.list, function(x) x[1,]))
      legend.col = unlist(lapply(legend.list, function(x) x[2,]))
    }
    else{legend.vec = legend.vec; legend.col=legend.col}

    idx = seq(0, length(legend.vec), by = (lnrow*lncol))
    lidx = length(idx)
    if(idx[length(idx)] < length(legend.vec) &  idx[length(idx)] > (lnrow*lncol) ){
      idx = c(idx , length(legend.vec))
    }

    if(lidx ==1){
      barplot(rep(1,length(soln)),col="white", border="white",axes=F, space=0 )
      legend("topright",legend=legend.vec, fill=legend.col, bty="n", ncol=lncol, cex=lcex)
    }

    if(lidx > 1){
      for(i in 1:lidx){
        barplot(rep(1,length(soln)),col="white", border="white",axes=F, space=0 )
        legend("topright",legend=legend.vec[(idx[i]+1):idx[i+1]], fill=legend.col[(idx[i]+1):idx[i+1]], bty="n", ncol=lncol, cex=lcex)
      }
    }
  }
  if(get.pval==TRUE){return(fmat)}
}

