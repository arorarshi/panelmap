#plot panelet containing continuous data

panelet_continuous<-function (pp, pp.col, gr=NULL, var.n = "var.n", NA.flag = TRUE,
                              NA.col = "grey", get.pval = FALSE, border = FALSE, border.col = "black",
                              ...)
{

  if(is.null(pp) | is.null(pp.col)){stop("pp and pp.col can't be NULL")}
  if(!is.null(gr)){
    if (any(is.na(pp)) & NA.flag == FALSE) {
      stop("There are NAs in data. Turn NA.flag=TRUE and specify NA.col. NA.col should be different than gradient of pp.col")
    }
    if (length(pp) != length(gr)) {
      stop("length of pp is not equal to gr vector")
    }
  }

  dots <- list(...)
  ii <- cut(pp, breaks = seq(min(pp, na.rm = T), max(pp, na.rm = T),
                             length.out = 100), include.lowest = TRUE)
  pcol <- colorRampPalette(pp.col)(99)[ii]
  n <- length(gr)
  #force border=FALSE
  if(is.null(gr)){n<-length(pp); border=FALSE}

  if (NA.flag == TRUE) {
    pcol[which(is.na(pcol))] = NA.col
  }
  barplot(rep(1, n), col = pcol, border = pcol, axes = F, space = 0)
  mtext(var.n, side = 2, las = 2, ...)

  if (get.pval == TRUE) {

    if(!(is.null(gr))){
      tab = get.summary2(gr, pp, 2, var.n = var.n)
      pval = tab[1, "pval"]
      mtext(paste0("P=", pval), side = 4, las = 2, ...)
    }

    if(is.null(gr)){
      tab = get.summary(pp,2,var.n=var.n)
      pval = unlist(lapply(strsplit(tab[1,1], ";"), function(x) x[1]))
      mtext(pval,side = 4, las = 2, ...)
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
  if (get.pval == TRUE) {
    return(list(key = NULL, tab = tab))
  }
  #return(list(key = NULL))
}
