\name{print-methods}
\docType{methods}
\alias{print-methods}
\alias{print,Fa-method}
\alias{print}

\title{Show/Print/Display an Object}

\description{
Show/print/display an object, including the Call, Standard deviations, Loadings.
}

\usage{
print(x, ...)
} 

\arguments{

    \item{x}{
an object of class \code{"Fa"} or of a class derived from \code{"Fa"}. 
}

    \item{...}{
additional arguments, e.g., print.x=TRUE
}

}

\section{Methods}{
\describe{

\item{x = "Fa"}{
generic functions - see \code{show}, \code{print}, \code{summary}, \code{predict}, \code{plot}, \code{getCenter}, \code{getEigenvalues}, \code{getFa}, \code{getLoadings}, \code{getQuan}, \code{getScores}, \code{getSdev}
}

}}

\value{
An invisible argument \code{x}.
}

\references{
Zhang, Y. Y. (2012), An Object Oriented Framework for Robust Factor Analysis.
}

\author{ Ying-Ying Zhang (Robert) \email{robertzhang@cqu.edu.cn} }

\seealso{\code{\link{Fa-class}}, \code{\link{SummaryFa-class}}}

\examples{
data(stock611)

facovRegOgk=FaCov(x=scale(stock611[,3:12]), factors=3, cov.control = CovControlOgk(), 
scores.method = "regression")

## object=show(object)=print(object)=myFaPrint(object)
## facovRegOgk is an object of class "Fa"
facovRegOgk
show(facovRegOgk)
print(facovRegOgk)
myFaPrint(facovRegOgk)

}

\keyword{methods}
