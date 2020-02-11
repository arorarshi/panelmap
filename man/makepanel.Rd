\name{makepanel}
\alias{makepanel}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A wrapper function to plot \code{panelmap} with one line command
}
\description{
A wrapper function to plot \code{panelmap} with one line command, which internally calls \code{panelet_category}, \code{panelet_group}, and \code{panelet_continuous}. One can pass all parameters in one go. For panelet level customization, use without wrapper function
}
\usage{
makepanel(soln, soln.name,soln.col,mat, mat.col,mat.type, legend=FALSE, get.pval=FALSE,NA.flag=FALSE, NA.col="grey", border=FALSE, border.col="black", lnrow=3, lncol=5,lcex=1, legend.vec=NULL, legend.col=NULL, lheight=1, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{

  \item{soln}{required. vector. Class or group labels. see \code{panelet_group}}
  \item{soln.name}{a string, name for your class or group labels}
  \item{soln.col}{a vector of colors specified for each class in \code{soln}. Also accepts vector from \code{RcolorBrewer}}
  \item{mat}{data frame. of values that you want to plot. Make sure they are ordered in the same order as \code{soln} vector.}
  \item{mat.col}{list. of color values to be coded. names of the list are column names of \code{mat} see example below.}
  \item{mat.type}{vector. type of data type in mat. \code{1 = discrete/binary}. \code{2 = continuous} .See example below.}
  \item{NA.flag}{logical, default is \code{FALSE}. see \code{get.colvector} for details}
  \item{NA.col}{default is "grey". see \code{get.colvector} for details}
  \item{get.pval}{logical, default is \code{FALSE}. Perform non parametric association tests to test for association between the group variable and the continuous variable of interest. If \code{TRUE} a table summarizing median value and range in each class is returned as well. }
  \item{border}{logical, default is \code{FALSE}. Add a border outside panelet and demarcating group boundaries }
  \item{border.col}{ default is \code{"black"}, if \code{border=TRUE}, you can specify the color of the border to be plotted. Plots outside the panelet and class boundaries}
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

}

\value{
A colored \code{panelemap} of discrete/continuous values is plotted ordered according to the group labels as specified.
}

\author{
Arshi Arora
}

\seealso{
\code{makepanel}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

#sort everything by cyl
mtcars.sort = mtcars[order(mtcars$cyl),]
#other features to plot
mat = mtcars.sort[,c("mpg","vs","am","gear")]

#color assigned to each feature
mat.col=list(am=c("white","black"), vs=c("white","black"), gear=c("yellow","orange","brown"), mpg=c("white", "darkred"))

#type of each feature. type=1 for discrete and type=2 for continuous.
mat.type= c(2,1,1,1)
tab=makepanel(soln=mtcars.sort$cyl, soln.name="cyl",soln.col=c("red","blue","green"), mat=mat, mat.col=mat.col, mat.type=mat.type, border=TRUE, legend=TRUE, get.pval=TRUE, cex=1)


#example to adjust for legend parameters
v1=v5= sample(c(1:3),50, replace=TRUE)
v2 = sample(c("aa","bb"),50, replace=TRUE)
v3 = v7 = v9 = sample(c(1:4), 50, replace=TRUE)
v4 = v6 = v8 = v10 = sample(c(1:6), 50, replace=TRUE)
mat = cbind(v2,v3,v4,v5,v6,v7,v8,v9,v10)
mat.type = rep(1,9)
col4 = c("cyan4", "coral3", "mediumpurple3", "azure3")
col6 = c("lightpink3", "lightpink4", "lightsalmon2", "lightsalmon3","lightsalmon4", "mistyrose3")
mat.col = list(v2 = c("white", "brown"), v3 = col4, v7 = col4, v9=col4, v4=col6, v6=col6, v8=col6, v10=col6,v5=c("mediumpurple3", "azure3", "lightsalmon4"))
v1 = v1[order(v1)]

makepanel(v1, "v1", c("red","blue","green"), mat, mat.col, mat.type, legend=TRUE, lnrow=2, lncol=4, lcex=1.2, lheight=4)

}
