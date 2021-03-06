\name{panelet_group}
\alias{panelet_group}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
plots group \code{panelet} row, the top row in \code{panelmap}
}
\description{
Given ordered class labels, make a row of colors grouped by the categories of class. This is the first plot in \code{makepanel}. All subsequent panelets should follow this ordering of class labels.

}
\usage{
panelet_group(gr, gr.col,gr.name="group",border=FALSE,
border.col="black", legend=FALSE, ...)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{gr}{a vector, of class labels in the form of character or integer}
  \item{gr.col}{a vector of colors specified for each class in \code{gr}. Also accepts vector from \code{RcolorBrewer}}
  \item{gr.name}{ a string, name of your group labels}
  \item{border}{logical. default is \code{FALSE}. If \code{TRUE} a border in black color is drawn across the color vector. }
  \item{border.col}{ default is \code{"black"}, if \code{border=TRUE}, you can specify the color of the border to be plotted. Plots outside the panelet and class boundaries}
  \item{legend}{logical. default is \code{FALSE}. if \code{TRUE} it returns a key specifying each color assigned to the class labels. Only displays in the plot with \code{makepanel} }
  \item{...}{pass optional arguments here}
}




\details{

\code{...} can pass other parameters. Use it for controlling names and colors of panelets via  \code{cex, col}. border line type and thickness can be controlled via \code{lty} and \code{lwd} respectively.

}
\value{
A colored \code{panelet} ordered by class labels is plotted.

\item{key}{ if legend = \code{TRUE}, returns a key mapping the group labels to specific color }

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
tt=panelet_group(gr=mtcars.sort$cyl, gr.col=c("red","blue","green"),
gr.name="cyl",cex=1, border=FALSE, legend=FALSE)

}
