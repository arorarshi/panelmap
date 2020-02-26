
#wrapper function - for groupless panelmap

makepanel_order<-function(orderl="user", mat, mat.col, mat.type, legend=FALSE, get.stat=FALSE, NA.flag=FALSE, NA.col="grey", lnrow = 3, lncol = 5,lcex = 1, legend.vec = NULL, legend.col = NULL, lheight = 1,...){

  dots<-list()

  if(orderl=="user"){

    tab=makepanel(mat=mat, mat.col=mat.col, mat.type=mat.type, legend=legend, get.pval=get.stat, NA.flag=NA.flag, NA.col=NA.col, border=FALSE, border.col="black", lnrow = lnrow, lncol = lncol,lcex = lcex, legend.vec = legend.vec, legend.col = legend.col, lheight = lheight,gr=NULL, ...)
    #colnames(tab) = "Summary";
    return(tab)

  }

  if(orderl == "bin"){

    if( !(sum(mat==0, na.rm=T) + sum(mat==1, na.rm=T) + length(which(is.na(mat))) == length(mat))) {stop("orderl=mut. mat can only contain 0s and 1s. There exists some other variable")}

    rs = apply(mat,1,sum); cs = apply(mat,2,sum)
    mat.order = mat[order(rs, decreasing=T), order(cs, decreasing=T)]

    tab=makepanel(mat=mat.order, mat.col=mat.col, mat.type=mat.type, legend=legend, get.pval=get.stat, NA.flag=NA.flag, NA.col=NA.col, border=border, border.col=border.col, lnrow = lnrow, lncol = lncol, lcex = lcex, legend.vec = legend.vec, legend.col = legend.col, lheight = lheight,gr=NULL, ...)

    #colnames(tab) = "Summary";
    return(list(tab = tab, mat.order=mat.order))

  }

}
