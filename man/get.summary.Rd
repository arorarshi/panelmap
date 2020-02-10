\name{get.summary}
\alias{get.summary}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Summarize continuous and dicrete variables.
}
\description{
A vector based summary function. Provides a table with percentages for dicrete data. And median, mean, range or standard deviation for continuous data.

}
\usage{
get.summary(var, type, type2="range")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{var}{ A vector, can be discrete(integer, character or factor) or continuous(numeric)}
  \item{type}{ integer}
    \code{type=1}{ if \code{x} is continuous }

    \code{type=2}{ if \code{x} is dicrete}

  \item{type2}{ if \code{type=2} specify whether you want range or standard deviation. default is - \code{"range"}, For sd pass \code{"sd"} }

}

\details{

Summary statistics are rounded to nearest 2 decimal points.
}
\value{
if \code{type=1} returns a character vector with summary information. length of the vector is equal to length of unique classes and \code{NA} if any.

if \code{type=2} returns a character of length 1 with summary information. \code{NAs} are separated by \code{";"}

}
\author{
Arshi Arora}

\seealso{
\code{get.summary2}

}


\examples{

#when type=1, data is discrete
set.seed(123)
x<-sample(1:3,50, replace=TRUE)
get.summary(x,type=1)

#lets see with NAs
x[c(1,5,30)] = NA
get.summary(x,type=1)

#when type=2, data is continuous
set.seed(123)
x<-rnorm(50)
get.summary(x, type=2, type2="range")

x[c(1,5,30)] = NA
get.summary(x, type=2, type2="sd")

}
