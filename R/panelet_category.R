#plot panelet containg two or more categories

panelet_category<-function (pp, pp.col, gr, var.n = "var.n", NA.flag = FALSE,
                            NA.col = "grey", get.pval = FALSE, border = FALSE, border.col = "black",
                            legend = FALSE, ...)
{

  if (length(pp) != length(gr) & !(is.null(gr))) {
    stop("length of pp is not equal to gr vector")
  }
  dots <- list(...)
  n <- length(gr)
  #force border=FALSE
  if(is.null(gr)){n<-length(pp); border=FALSE}
  pcol <- get.colvector(pp, pp.col, NA.flag, NA.col)
  barplot(rep(1, n), col = pcol$labels.col, border = pcol$labels.col,
          axes = F, space = 0)
  mtext(var.n, side = 2, las = 2, ...)
  if (get.pval == TRUE) {

    if(!(is.null(gr))){
      tab = get.summary2(gr, pp, 1, var.n = var.n)
      pval = tab[1, "pval"]
      mtext(paste0("P=", pval), side = 4, las = 2, ...)
    }

    if(is.null(gr)){
      tab = get.summary(pp,1,var.n=var.n)
      if(length(unique(na.omit(pp))) ==2){
        pval = tab[2,1]
        mtext(pval,side = 4, las = 2, ...)
      }

    }
  }
  if (border == TRUE) {
    dots2 = dots
    dots2$v = 0
    dots2$col = border.col
    do.call(abline, args = dots2)
    at.points = cumsum(table(gr))
    for (i in 1:length(at.points)) {
      dots2 = dots
      dots2$v = at.points[i]
      dots2$col = border.col
      do.call(abline, args = dots2)
    }
  }
  key = pcol$key
  if (legend == TRUE & get.pval==FALSE) {
    key[1, ] = paste0(var.n, "=", key[1, ])
    return(list(key = key))
  }
  if (get.pval == TRUE & legend==FALSE) {
    return(list(tab = tab))
  }

  if (get.pval == TRUE & legend==TRUE) {
    key[1, ] = paste0(var.n, "=", key[1, ])
    return(list(key = key, tab = tab))
  }

}
