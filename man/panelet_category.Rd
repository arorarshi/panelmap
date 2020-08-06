\name{panelet_category}
\alias{panelet_category}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
plots categorical values in the order specified by class labels
}
\description{
Plot discrete categorical values belonging to class labels. Each category can have a unique color and a color for missing if exists. User has the option to perform an association test and output p-value and a contingency table summarizing count and proportion.
}
\usage{
panelet_category(pp,pp.col,gr, var.n="var.n",NA.flag=FALSE,
NA.col="grey", get.pval=FALSE, border=FALSE, border.col="black", legend=FALSE,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pp}{required. vector. A vector of categorical values of interest }
  \item{pp.col}{required. vector. A vector of colors of the same length as the unique values in the categorical variable of interest}
  \item{gr}{required. vector. Class labels as passed to \code{panelet_group}. If \code{NULL} assumes no grouping, and outputs \code{median and range} if \code{get.pval=TRUE}}
  \item{var.n}{character. A character specifying the name of the variable. Set to \code{blank ""} if you want to omit printing any text. See \code{cex}}
  \item{NA.flag}{logical, default is \code{FALSE}. see \code{get.colvector} for details}
  \item{NA.col}{default is "grey". see \code{get.colvector} for details}
  \item{get.pval}{logical, default is \code{FALSE}. Perform Fisher's Exact Test to test for association between the group variable and the categorical variable of interest. If \code{TRUE} a table summarizing count and proportion is returned as well. }

  \item{border}{logical, default is \code{FALSE}. Add a border outside panelet and demarcating group boundaries }
  \item{border.col}{ default is \code{"black"}, if \code{border=TRUE}, you can specify the color of the border to be plotted. Plots outside the panelet and class boundaries}
  \item{legend}{logical. default is \code{FALSE}. if \code{TRUE} it returns a key specifying each color assigned to the categories. Only displays in the plot with \code{makepanel}}
   \item{...}{pass optional arguments here}

}


\details{

\code{...} can pass other parameters. Use it for controlling names and colors of panelets via  \code{cex, col}. border line type and thickness can be controlled via \code{lty} and \code{lwd} respectively.

A colored \code{panelet} of categorical values is plotted. Make sure the variable is ordered according to the group labels in \code{panelet_group}.
}

\value{
A colored \code{panelet} of categorical values is plotted.

\item{key}{ if \code{legend = TRUE}, returns a key mapping the group labels to specific color }

\item{tab}{ if  \code{get.pval =TRUE}, returns a table summarizing the counts and proportions along with a p-value from Fisher's Exact Test }


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
par(mfrow=c(5,1),mar=c(0,8,0,14))

#sort the entire data frame according to the group solution you are interested in
mtcars.sort = mtcars[order(mtcars$cyl),]

#plot group
panelet_group(gr=mtcars.sort$cyl, gr.col=c("red","blue","green"),
gr.name="cyl",cex=2, border=TRUE, legend=FALSE)

}
