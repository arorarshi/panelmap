##################################
# Arshi Arora
#summary table 1 functions:
# 1. get.summary
# 2. get.summary2
#3. get.colvector
##################################


##################################
#summarize vector values, can be discrete or continuous.
#continuous - median(range) or median(sd)
#discrete - tabbulate

get.summary<-function(var,type, var.n=NULL, type2="range"){
  if(type==1){
    #discrete
    tt = table(as.character(var),useNA="ifany")
    ttf = round(prop.table(tt)*100,2)
    summary = mapply( function(x,y) paste0(x,"(",y,"%)"),tt,ttf)
    summary = as.matrix(summary); rownames(summary)[1] = paste0(var.n, "=", rownames(summary)[1])
    return(summary)
  }

  if(type==2){
    #continuous
    if(length(var) ==1){warning("trying to summarize only 1 variable")}
    var = as.numeric(as.character(var))
    median.value = round(median(var, na.rm=T),2)
    if(type2=="range"){
      range.value = round(range(var, na.rm=T),2)
      tt = paste0(median.value,"(",range.value[1],",",range.value[2] ,"); NA=",length(which(is.na(var))) )}
    if(type2=="sd"){
      sd.value = round(sd(var, na.rm=T),2)
      tt=paste0(median.value,"(",sd.value,"); NA=",length(which(is.na(var))) )}

    tt=as.matrix(tt)
    rownames(tt) = var.n
    return(tt)
  }
}

##################################
#association tests of continuous and discrete variables in a neat table format


get.summary2<-function(fac,var,type,test.type="np",skip.test=FALSE, var.n=NULL){

  if(length(fac) != length(var)){ stop("unequal lengths of fac and var")}
  if(type==2){if(length(which(is.na(fac))) !=0){ stop("Factor variable cannot have NA")}}

  #categorical data
  if (type==1){
    fac = as.character(fac)
    var = as.character(var)
    x = table(var,fac,useNA="ifany")
    #get row sum and freq
    csum = apply(x,2,sum)
    rsum = apply(x,1,sum)
    xperc = apply(x,2,prop.table)
    xperc = round(xperc,2)
    xtab = matrix("",nrow=nrow(x),ncol=ncol(x))
    for (i in 1:nrow(x)){
      for(j in 1:ncol(x)){
        xtab[i,j] = as.character(paste0(x[i,j],"(",xperc[i,j]*100,"%)"))
      }
    }
    rownames(xtab) = names(rsum)
    colnames(xtab) = mapply(function(a,b) paste0(a,"(n=",b,";", round(((b/sum(csum)) * 100),2), "%)"),colnames(x), csum)

    rs = rep("", (length(rsum)+1))
    pval= rep("",length(rs))
    var.name = rep("", ncol(xtab))

    #fisher test handles NA
    if(skip.test==FALSE){
      message("Performing Fisher's Exact test")
      pp = fisher.test(fac,var,workspace=2e8)$p.value
      pp = ifelse(pp<0.0001, "P<0.0001", round(pp,digits=4))
      pval[1] = pp}

    for (i in 1:length(rsum)){
      rs[i+1] = as.character(paste0(rsum[i],"(",round((rsum[i]/sum(rsum)*100),2),"%)"))
    }

    fmat = (cbind(rbind(var.name,xtab),rs,pval))
    colnames(fmat)[which(colnames(fmat)=="rs")] = "RowTotal"

    if(!(is.null(var.n))){ rownames(fmat)[which(rownames(fmat) == "var.name")] = var.n  }

  }

  #continuous
  if (type==2){
    fac = as.character(fac)
    var = as.numeric(as.character(var))

    mm =cbind.data.frame(fac, var, stringsAsFactors=F)

    if(test.type=="p"){

      tt.summary = ddply(mm, c("fac"), summarize, mean=mean(var,na.rm=T),
                         sd=sd(var,na.rm=T), NAs = length(which(is.na(var))) )
    }

    if(test.type=="np"){
      tt.summary = plyr::ddply(mm, c("fac"), summarize, median=median(var,na.rm=T),
                               min=range(var,na.rm=T)[1],
                               max=range(var,na.rm=T)[2], NAs = length(which(is.na(var))) )
    }



    cnames = as.character(tt.summary[,1])
    tt.summary = tt.summary[,-1]
    tt.summary = apply(tt.summary, 2,function(x) round(as.numeric(as.character(x)),2))

    if(test.type=="np"){fmat = mapply( function(x,y,z) paste0(x,"[",y,"-",z,"]"),tt.summary[,1],tt.summary[,2], tt.summary[,3])}

    if(test.type=="p"){fmat = mapply( function(x,y,z) paste0(x,"(",y,")"),tt.summary[,1],tt.summary[,2])}

    #y is a numeric, x is a factor wilcox_test(y~x)
    #always perform wilcox test for ties
    if (skip.test==FALSE){

      way = length(unique(na.omit(fac)))
      if(way ==1){ stop("fac has only one class of variables to compare? Need atleast 2 levels")}
      if(way==2 & test.type =="p"){message("Performing T-Test")
        pval =t.test(var ~ as.factor(fac))$p.value }
      if(way==2 & test.type =="np"){ message("Performing Wilcoxon Rank Sum test")
        pval = coin::pvalue(coin::wilcox_test(var ~ as.factor(fac)))}
      #one way aov
      if(way>2 & test.type =="p"){ message("Performing one way ANOVA")
        pval = summary(aov(var ~ as.factor(fac)))[[1]][[5]][1]}
      if(way>2 & test.type =="np"){message("Performing Kruskal-Wallis test")
        pval = kruskal.test(var, as.factor(fac))$p.value}

      pval = ifelse(pval<0.0001, "P<0.0001", round(pval,digits=3))
    }

    if(skip.test==TRUE){ pval = NA}

    fmat = c(fmat,"",pval)
    names(fmat) = c(cnames,"RowTotal","pval")
    nas = tt.summary[,"NAs"]
    fmat = rbind(fmat,c(nas,"",""))
    rownames(fmat)= c("var.name", "NA")

    if(!(is.null(var.n))){ rownames(fmat)[which(rownames(fmat) == "var.name")] = var.n  }
  }

  return(fmat)
}


##################################
#get a color vector with match and NA
get.colvector<-function(labels,col, NA.flag=FALSE, NA.col="grey"){

  if(!NA.flag & anyNA(labels)){stop("NAs exist in your labels. pass NA.flag==TRUE!")}

  if(NA.flag & anyNA(labels)){col <- c(col, NA.col)}

  if(NA.flag & !anyNA(labels)){message("No NAs found in labels")}

  if (length(unique(labels)) != length(col)){ stop("Unequal length of unique labels and color")}
  if (anyDuplicated(col)) { stop("Duplicated colors for some values. check col parameter")}

  ul<-sort(unique(labels), na.last=TRUE)
  #if(is.factor(labels)){ul<-levels(labels)} - sort works on factor, but check later
  #use match
  labels.col <- col[match(labels, ul)]

  if(!is.null(names(labels))){names(labels.col) <- names(labels)}
  key <- rbind(as.character(ul), col)
  labels.list<-list(labels.col=labels.col, key=key)
  return(labels.list)
}

