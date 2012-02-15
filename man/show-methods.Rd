\name{show-methods}
\docType{methods}
\alias{show-methods}
\alias{show,Fa-method}
\alias{show,SummaryFa-method}
\alias{show}

\title{Show/Print/Display an Object}

\description{
Show/print/display an object, including the Call, Standard deviations, Loadings.
}

\usage{
show(object)
} 

\arguments{

    \item{object}{
an object of class \code{"SummaryFa"}, \code{"Fa"}, or of a class derived from \code{"Fa"}.
}

}

\section{Methods}{
\describe{

\item{object = "Fa"}{
generic functions - see \code{show}, \code{print}, \code{summary}, \code{predict}, \code{plot}, \code{getCenter}, \code{getEigenvalues}, \code{getFa}, \code{getLoadings}, \code{getQuan}, \code{getScores}, \code{getSdev}
}

\item{object = "SummaryFa"}{
generic functions - see \code{show}
}

}}

\value{
An invisible argument \code{object}.
}

\references{
Zhang, Y. Y. (2012), An Object Oriented Framework for Robust Factor Analysis.
}

\author{ Ying-Ying Zhang (Robert) \email{robertzhang@cqu.edu.cn} }

\seealso{\code{\link{Fa-class}}, \code{\link{SummaryFa-class}}}

\examples{
data(stock611)

facovRegOgk=FaCov(x=scale(stock611[,3:12]), factors=3, cov.control = CovControlOgk(), 
scoresMethod = "regression")

## object=show(object)=print(object)=myFaPrint(object)
## facovRegOgk is an object of class "Fa"
facovRegOgk
show(facovRegOgk)
print(facovRegOgk)
myFaPrint(facovRegOgk)

## summaryFacovRegOgk is an object of class "SummaryFa"
summaryFacovRegOgk=summary(facovRegOgk); summaryFacovRegOgk
show(summaryFacovRegOgk)

}

\keyword{methods}
