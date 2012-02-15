\name{myFaPrint}
\alias{myFaPrint}

\title{Show/Print/Display an Object}

\description{
Show/print/display an object, including the Call, Standard deviations, Loadings, and Rotated variables (if \code{print.x = TRUE}).
}

\usage{
myFaPrint(object, print.x=FALSE)
} 

\arguments{

    \item{object}{
an object of class \code{"Fa"} or of a class derived from \code{"Fa"}. 
}

    \item{print.x}{
Logical. If \code{print.x = TRUE}, then print the rotated variables (scores).
}

}

\value{
An invisible argument \code{object}.
}

\references{
Zhang, Y. Y. (2012), An Object Oriented Framework for Robust Factor Analysis.
}

\author{ Ying-Ying Zhang (Robert) \email{robertzhang@cqu.edu.cn} }

\seealso{\code{\link{Fa-class}}}

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

}

\keyword{robust}
