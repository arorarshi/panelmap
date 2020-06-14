\name{circomap}
\alias{circomap}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
circular layout of more than one \code{panlemaps}
}
\description{
\code{circomap} plots multiple cohorts having grouped data in the form of a \code{panelmap} laid out in a circular fashion. Each pie reflects the relative size of each cohort, and each track is a panelet summarizing a feature.
}
\usage{
circomap(datasets, gtoplot, gcol, gheight, ftoplot, ftype, fcol, fheight, NA.flag="FALSE", NA.col="grey")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{datasets}{

  list. Arrange the datasets to plot in a list. Each dataset has samples as columns and features as rows. The row features should be uniform across datasets. See \code{example} below. The dataset is sorted according to the group variable and is included in the dataset.
}
  \item{gtoplot}{
  character. Name of the group variable to plot.
}
  \item{gcol}{

  vector. Color to be assigned to each group in group labels.
}
  \item{gheight}{

  numeric. Height of the group label panelet track. Default is \code{0.10}
}
  \item{ftoplot}{
  vector. Names of the features to plot. See \code{example}
}
  \item{ftype}{
vector. Type of data type to be plotted in \code{ftoplot}. \code{1 = discrete/binary}. \code{2 = continuous} .See example below and \code{makepanel}.

}
  \item{fcol}{
list. A list of color values to be coded. Names of the list are same as \code{ftoplot} see example below and \code{makepanel}.
}
  \item{fheight}{
numeric. Height of each of the panelet track.
}
\item{NA.flag}{
logical. default is FALSE. If there are NAs in your categories set it as TRUE
}
\item{NA.col}{
character. Default is "grey". Set the color for NAs
}
}
\details{

The radius of circomap is 1. Heights of \code{gheight} and \code{fheight} should be chosen accordingly.
}
\value{
returns a \code{circomap}
}
\references{
Gu, Z. circlize implements and enhances circular visualization in R. Bioinformatics 2014.
}
\author{
Arshi Arora
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{

\code{makepanel}
}
\examples{

#dataset 1
dat1 = mtcars[order(mtcars$cyl),]
#dataset 2
dat2 = mtcars[order(mtcars$cyl),]
#dataset 3
dat3 = mtcars[order(mtcars$cyl),]

#put all the datasets in a list
datasets<-list()
datasets[[1]]=dat1; datasets[[2]]=dat2; datasets[[3]]=dat3
names(datasets) = c("dat1", "dat2", "dat3")
#names of datasets will be used to labek the datasets

#group variable to plot same as panelet_group
gtoplot<-c("cyl")
gcol = c("red","blue","green")
gheight = 0.10 #track height of the group variable

#features to plot - same as in panelmap example
ftoplot<-c("mpg", "vs", "am", "gear")
ftype= c(2,1,1,1)
fcol=list(am=c("white","black"), vs=c("white","black"), gear=c("yellow","orange","brown"), mpg=c("white", "darkred"))

fheight<-list(); fheight[1:length(ftoplot)] = 0.08
#Voila!!
circomap(datasets, gtoplot, gcol, gheight, ftoplot, ftype, fcol, fheight)

}
