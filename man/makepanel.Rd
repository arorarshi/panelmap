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
makepanel(soln, soln.name, soln.col, mat, mat.col, mat.type, legend = FALSE, get.pval = FALSE, NA.flag = FALSE, NA.col = "grey", border = FALSE, border.col="black", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{soln}{
required. vector. Class/group labels. see \code{panelet_group}
}
  \item{soln.name}{
a string, name for your class/group labels
}
  \item{soln.col}{
a vector of colors specified for each class in \code{soln}. Also accepts vector from \code{RcolorBrewer}
}
  \item{mat}{
data frame. of values that you want to plot. Make sure they are ordered in the same order as \code{soln} vector.
}
  \item{mat.col}{
 list. of color values to be coded. names of the list are column names of \code{mat} see example below.
}
  \item{mat.type}{
 vector. type of data type in mat. \code{1 = discrete/binary}. \code{2 = continuous} .See example below.
}
\item{NA.flag}{logical, default is \code{FALSE}. see \code{get.colvector} for details}
  \item{NA.col}{default is "grey". see \code{get.colvector} for details}
  \item{get.pval}{logical, default is \code{FALSE}. Perform non parametric association tests to test for association between the group variable and the continuous variable of interest. If \code{TRUE} a table summarizing median value and range in each class is returned as well. }
  \item{border}{logical, default is \code{FALSE}. Add a border outside panelet and demarcating group boundaries }
  \item{border.col}{ default is \code{"black"}, if \code{border=TRUE}, you can specify the color of the border to be plotted. Plots outside the panelet and class boundaries}
   \item{legend}{logical. default is \code{FALSE}. if \code{TRUE} plots a legend at bottom right corned specifying each color assigned to the categories.}
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
#otehr features to plot
mat = mtcars.sort[,c("mpg","vs","am","gear")]

#color assigned to each feature
mat.col=list(am=c("white","black"), vs=c("white","black"), gear=c("yellow","orange","brown"), mpg=c("white", "darkred"))

#type of each feature. type=1 for discrete and type=2 for continuous.
mat.type= c(2,1,1,1)
tab=makepanel(soln=mtcars.sort$cyl, soln.name="cyl",soln.col=c("red","blue","green"), mat=mat, mat.col=mat.col, mat.type=mat.type, border=TRUE, legend=TRUE, get.pval=TRUE, cex=2)
## Performing Kruskal-Wallis test
## Performing Fisher's Exact test
## Performing Fisher's Exact test
## Performing Fisher's Exact test

}
