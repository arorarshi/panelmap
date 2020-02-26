\name{makepanel_order}
\alias{makepanel_order}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A \code{makepanel} functionality with user defined order or when no group is specified or discrete data.
}
\description{
A \code{makepanel} functionality with user defined order or when no group is specified. This also caters to mutation data or discrete data, and tries to arrange panlemap by decreasing frequency of features and columns.

}
\usage{
makepanel_order(orderl="user", mat, mat.col, mat.type, legend=FALSE, get.stat=FALSE, NA.flag=FALSE, NA.col="grey", lnrow = 3, lncol = 5,lcex = 1, legend.vec = NULL, legend.col = NULL, lheight = 1,...)
}

\arguments{

  \item{orderl}{required. default is \code{"user"}. \code{"user"} means it assumes the given matrix is in user defined order and just plots it as is. \code{"bin"} means it sis a binary matrix fully consisting of 0 and 1, and will order it in the order of decreasing sum of row and column. }
  \item{mat}{required. data frame. of values that you want to plot. Make sure they are ordered in the same order as \code{gr} vector.}
  \item{mat.col}{required. list. of color values to be coded. names of the list are column names of \code{mat} see example below.}
  \item{mat.type}{required. vector. type of data type in mat. \code{1 = discrete/binary}. \code{2 = continuous} .See example below.}
  \item{NA.flag}{logical, default is \code{FALSE}. see \code{get.colvector} for details}
  \item{NA.col}{default is "grey". see \code{get.colvector} for details}
  \item{get.stat}{logical, default is \code{FALSE}. Summarizes continuous data as \code{median(range)}. Categorical variables with 2 categories are summarized as \code{n(\%)} If \code{TRUE} a table with these summary statistics is returned as well. }
  \item{legend}{logical. default is \code{FALSE}. if \code{TRUE} plots a legend at bottom right corned specifying each color assigned to the categories.}
  \item{lnrow}{numeric. Number of rows in each panelet of the legend. Adjust to fit in plot margins. Default is 3 rows. See \code{example}. Only when \code{legend=TRUE}}
  \item{lncol}{numeric. Number of columns on each panelet of the legend. Adjust accordingly for legibility. Default is 5 columns.Only when \code{legend=TRUE}}
  \item{lcex}{numeric. Magnification of legend labels. Default value is \code{cex=1}. Only when \code{legend=TRUE}}
  \item{legend.vec}{vector. When \code{legend=TRUE}, all values passed as \code{mat.type=1} are passed to be plotted in the legend. If you want to show less information,pass the legend vector of your choice.}
  \item{legend.col}{vector. When \code{legend=TRUE}, all values passed as \code{mat.type=1} are given a color and are plotted in the legend. If you want to show less information, pass the legend vector colors of your choice corresponding to \code{legend.vec}.}
  \item{lheight}{numeric. If your legend overflows the current \code{panelmap} margins, use this to add more space for legend. Default value is 1.Only when \code{legend=TRUE}}
  \item{...}{pass optional arguments here}
}


\details{

\code{...} can pass other parameters. Use it for controlling names and colors of panelets via  \code{cex, col}. border line type and thickness can be controlled via \code{lty} and \code{lwd} respectively.

See \code{makepanel}
}

\value{
A colored \code{panelemap} of discrete/continuous values is plotted ordered according to the input data.
if \code{get.stat==TRUE and orderl == "user"}
 \code{tab} of summary statistics is returned. See \code{get.stat}
if(orderl == "bin")
\code{tab} of summary statistics is returned. See \code{get.stat} and
\code{mat.order} an ordered data matrix is returned
}

\author{
Arshi Arora
}

\seealso{
\code{makepanel}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

#Simulate a 0 and 1 10x10 matrix
mat = matrix(sample(c(0,1),100, replace=TRUE), 10,10)
rownames(mat) = paste0("S", 1:10)
colnames(mat) = paste0("F", 1:10)
mat.type=rep(1, ncol(mat))

cc = c("white", "black")
mat.col<-list(); mat.col[[1]] = cc; mat.col = rep(mat.col,ncol(mat)); names(mat.col) = colnames(mat)

makepanel_order(orderl="bin", mat=mat, mat.col=mat.col, mat.type=mat.type, legend=TRUE, get.stat=TRUE)

}
