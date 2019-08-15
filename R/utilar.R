##################################
# Arshi Arora
#list of functions in this file:
# 1. replaceNAs
# 2. my.table
# 3. get.colvector
##################################


##################################
#replace NAs as NA in a vector when coded like - 9 N/A Unknown, Not Available etc
replaceNAs<-function(x,vNA){

  if(length(vNA)==0){stop("Provide values to be replaced by NA")}
  cc<-class(x)
  #convert everything to character
  x<-as.character(x)
  vNA<-as.character(vNA)

  for (i in 1:length(vNA)){
    x[which(x %in% vNA[i] )] = NA
  }
  #return back original class of vector
  x.na<-type.convert(x,cc)
  return(x.na)
}

type.convert<-function(x,cc){

  if(cc == "character"){return(as.character(x))}
  if(cc == "double"){return(as.double(x))}
  if(cc == "factor"){ warning("A typeof factor was returned. check for inconsistencies")
                      return(as.factor(as.character(x))) }
  if(cc == "integer"){return(as.numeric(x))}
  else{return(as.character(x))}
}



##################################
#get a color vector for a heatmap, panels etc, add grey for missing
get.colvector<-function(labels,col, NA.flag=FALSE, NA.col="grey"){

  if(NA.flag==FALSE){
    if (length(unique(labels)) != length(col)){ stop("Unequal length of unique labels and color")}
  }

  if(NA.flag==TRUE){
    if (length(unique(na.omit(labels))) != length(col)){ stop("Unequal length of unique labels and color")}
  }

  if (NA.flag==TRUE){ inter = intersect(col, NA.col)
    if(length(inter)!=0){
      print(paste0("NA.col = ", NA.col," is repeated in col"))
      stop("Choose another color for NA.col")}
  }

  ul<-sort(unique(labels))
  if(is.factor(labels)){ul<-levels(labels)}

  labels.col= rep(NA,length(labels))
  for(i in 1:length(ul)){
    labels.col[as.character(labels)==as.character(ul[i])] = col[i]
  }
  if(NA.flag==TRUE){labels.col[which(is.na(labels.col))]=NA.col
    ul<-c(ul,"NA")
    col<-c(col, NA.col)}

  if(!is.null(names(labels))){names(labels.col) = names(labels)}
  key = rbind(ul, col)
  labels.list<-list(labels.col=labels.col, key=key)
  return(labels.list)
}


##################################
# tab by overlap

my.table<-function(x,y){

  if( is.null(names(x))  | is.null(names(y)) ){stop("vectors have no names")}
  inter = intersect(names(x),names(y))
  if(length(inter)==0){stop("No overlapping names")}
  x = x[inter]
  y = y[inter]

  print(table(x,y,useNA="ifany"))

}

