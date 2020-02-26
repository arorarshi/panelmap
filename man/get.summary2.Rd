\name{get.summary2}
\alias{get.summary2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
summarizes and does association test for 2 or more variables
}
\description{
Summarize and provide p-value after association test of 2 or more variables. Percentages are reported for discrete variables. Users can choose the type of test they want. For continuous variable median and range are reported if the test specified is non parametric, mean and standard deviation are reported for a parametric test.  For discrete categories we perform Fisher's Exact Test, for continuous following options are available - parametric - Students' T-Test or one way ANNOVA, for non-parametric Wilcoxon Rank sum test or Kruskal Wallis test.
}
\usage{
get.summary2(fac,var,type,test.type="np",skip.test=FALSE, var.n=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fac}{ vector, a discrete vector of interest for which association is to be tested against other categorical or continuous variables}
  \item{var}{ vector, can be discrete or continuous}
  \item{type}{ \code{type=1} - meaning both \code{fac} and \code{var} are categorical, and Fisher's Exact test will be performed

    \code{type=2} - \code{var} is continuous and you can choose from available parametric (Students' T-Test or one way ANNOVA) and non parametric tests. (Wilcoxon Rank sum test or Kruskal Wallis test.)}

  \item{test.type}{possible values - \code{"np"} or \code{"p"}, Default is \code{"np"} (non parametric). Use \code{"p"} for parametric tests}
  \item{skip.test}{ logical, Default is \code{"TRUE"}. A association test is always performed. However, if there are too many categories in \code{fac}, it might slow down computation required for Fishers Exact test, one can decide to skip test by \code{skip.test=FALSE} to prevent limbo. }
  \item{var.n}{ character, If a variable name is provided, it will add this to output. }

}
\details{
Summary statistics are rounded to nearest 2 decimal points.
Note that length of \code{fac} and \code{var} should be the same. For Wilcoxon Rank sum test ties are handled through \code{wilcox.test} function in \code{coin} package.
workspace parameter in Fisher's Exact test is set to \code{workspace=2e8}. All p-values < 0.0001 are replaced with a character string \code{P<0.0001 or otherwise rounded to three decimal points. }
}


\value{

\item{fmat}{returns a character matrix summarizing \code{fac} and \code{var}. See \code{examples}}

}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Arshi Arora}


\seealso{
\code{get.summary}

}
\examples{

#two categorical variables
set.seed(100)
fac<-sample(1:2, 50, replace=TRUE)
var<-sample(1:3, 50, replace=TRUE)
get.summary2(fac,var, type=1,var.n="Example, Type=1" )


#continuous with two categories of fac
var<-rnorm(50)
get.summary2(fac, var,  type=2, test.type="p", var.n="2-Way, test.type=p")
get.summary2(fac, var,  type=2, test.type="np", var.n="2-Way, test.type=np")

#continuous with three categories of fac
fac<-sample(1:3, 50, replace=TRUE)
get.summary2(fac, var,  type=2, test.type="p",var.n="Example, test.type=p")
get.summary2(fac, var,  type=2, test.type="np",var.n="Example, test.type=np")


}
