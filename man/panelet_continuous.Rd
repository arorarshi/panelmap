\name{panelet_continuous}
\alias{panelet_continuous}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
plots continuous value as a gradient in the order specified by class labels
}
\description{
Plot continuous values belonging to class labels. The values are plotted relative to each other in a gradient from a spectrum between two colors. User has the option to perform an association test and output p-value and a table summarizing median values and range.


}
\usage{
panelet_continuous<-function(pp,pp.col,soln, var.n="var.n",labels.col="black",NA.flag=TRUE, NA.col="grey", get.pval=FALSE, border=FALSE, border.col="black", ...)
}

%- maybe also 'usage' for other objects documented here.
\arguments{

\item{pp}{required. vector. A vector of continuous values of interest }
  \item{pp.col}{required. vector. A vector of colors out of which a spectrum of relative values will be plotted to make a gradient of continuous values in \code{pp}}
  \item{soln}{required. vector. Class labels as passed to \code{panelet_group}}
  \item{var.n}{character. A character specifying the name of the variable. Set to \code{blank ""} if you want to omit printing any text. See \code{cex}}
  \item{labels.col}{character. default is \code{"black"}. control the color of \code{var.n} and p-value to be printed. }
  \item{NA.flag}{logical, default is \code{FALSE}. see \code{get.colvector} for details}
  \item{NA.col}{default is "grey". see \code{get.colvector} for details}
  \item{get.pval}{logical, default is \code{FALSE}. Perform non parametric association tests to test for association between the group variable and the continuous variable of interest. If \code{TRUE} a table summarizing median value and range in each class is returned as well. }
  \item{border}{logical, default is \code{FALSE}. Add a border outside panelet and demarcating group boundaries }
  \item{border.col}{ default is \code{"black"}, if \code{border=TRUE}, you can specify the color of the border to be plotted. Plots outside the panelet and class boundaries}
  \item{...}{pass optional arguments here}
}




\details{

\code{...} can pass other parameters. Use it for controlling names and colors of panelets via  \code{cex, col}. border line type and thickness can be controlled via \code{lty} and \code{lwd} respectively.


A colored \code{panelet} of continuous values is plotted as a gradient with values relative to each other. Make sure the variable is ordered according to the group labels in \code{panelet_group}.
}

\value{
A colored \code{panelet} of categorical values is plotted.

\item{tab}{ if  \code{get.pval=TRUE}, returns a table summarizing the median value and range with a p-value from Kruskal Wallis Test }


}

\author{
Arshi Arora
}

\seealso{
\code{makepanel}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{

#adjust margins and number of panelet values
par(mfrow=c(6,1),mar=c(0,8,0,14))

#sort the entire data frame according to the group solution you are interested in
mtcars.sort = mtcars[order(mtcars$cyl),]

#plot group
tt=panelet.group(soln=mtcars.sort$cyl, soln.col=c("red","blue","green"),soln.name="cyl",cex=1, border=TRUE, legend=FALSE)

#plot variable mpg
tt =panelet.continuous(mtcars.sort$mpg, pp.col=c("white","darkred"), soln=mtcars.sort$cyl,var.n="mpg", labels.col="black",NA.flag=FALSE, get.pval=TRUE, border=TRUE)
## Performing Kruskal-Wallis test
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
