#circomap function - cretaes multiple panelmaps in a circular layout


circomap<-function(datasets, gtoplot, gcol, gheight, ftoplot, ftype, fcol, fheight){

  #collect all feeatures across datsets
  ff=NULL
  subs.seq = NULL
  group = NULL
  fmat = NULL

  for(i in 1:length(datasets)){

    ff = c(ff, rep( names(datasets)[i], nrow(datasets[[i]]) ) )
    subs.seq = c(subs.seq, 1:nrow(datasets[[i]]) )
    group = c(group, datasets[[i]][,gtoplot])

    tt = matrix(NA, nrow=nrow(datasets[[i]]), ncol=length(ftoplot))
    for(j in 1:length(ftoplot)){
      tt[,j] = datasets[[i]][,ftoplot[j]]
    }
    fmat = rbind(fmat, tt)

  }

  #local functions in side the function to plot
  #as this.col i sa local variable for each ftoplot
  circlet.category<-function(x){return(get.colvector(x,this.col)$labels.col)}

  circlet.continuous<-function(x){
    ii <- cut(x, breaks = seq(min(x,na.rm=T), max(x,na.rm=T), length.out = 100), include.lowest = TRUE)
    ## Use bin indices, ii, to select color from vector of n-1 equally spaced colors
    mm <- colorRampPalette(this.col)(99)[ii]
    return(mm)
  }

  #initialize circomap
  circlize::circos.initialize(factors = ff, x=subs.seq)

  #plot the group variable
  this.col = gcol
  circlize::circos.track(ff, x=group, ylim=c(0,1), panel.fun = function(x,y){
    xlim = CELL_META$xlim
    ylim = CELL_META$ylim
    breaks = seq(1, length(x), by=1)
    n_breaks = length(breaks)
    circos.rect(breaks[-n_breaks], rep(ylim[1], n_breaks - 1),
                breaks[-1], rep(ylim[2], n_breaks - 1),
                col = circlet.category(x), border = NA)
    circos.text(CELL_META$xcenter, CELL_META$cell.ylim[2] + uy(5, "mm"),
                CELL_META$sector.index)
  }, bg.border=NA, track.height=gheight)

  #now start plotting remaining features
  for(i in 1:length(ftoplot)){

    this.col = fcol[[ftoplot[i] ]]
    this.type = ftype[i]
    ff.value = fmat[,i]
    this.fheight = fheight[[i]]

    if(this.type==1){
      circlize::circos.track(ff, x=ff.value, ylim=c(0,1), panel.fun = function(x,y){

        xlim = CELL_META$xlim
        ylim = CELL_META$ylim

        breaks = seq(1, length(x), by=1)
        n_breaks = length(breaks)

        circos.rect(breaks[-n_breaks], rep(ylim[1], n_breaks - 1),
                    breaks[-1], rep(ylim[2], n_breaks - 1),
                    col = circlet.category(x), border = NA)

      },track.height=this.fheight)


    }

    if(this.type==2) {

      circlize::circos.track(ff, x=ff.value, ylim=c(0,1), panel.fun = function(x,y){

        xlim = CELL_META$xlim
        ylim = CELL_META$ylim

        breaks = seq(1, length(x), by=1)
        n_breaks = length(breaks)

        circos.rect(breaks[-n_breaks], rep(ylim[1], n_breaks - 1),
                    breaks[-1], rep(ylim[2], n_breaks - 1),
                    col = circlet.continuous(x), border = NA)

      },track.height=this.fheight)

    }

  }

#end of circomap
}
