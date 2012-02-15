\name{predict-methods}
\docType{methods}
\alias{predict-methods}
\alias{predict,Fa-method}
\alias{predict}

\title{Calculates prediction}

\description{
Calculates prediction using the results in object. The newdata argument is an optional data frame or matrix in which to look for variables with which to predict. If newdata is omitted, the scores are used.
}

\usage{
predict(object, ...)
} 

\arguments{

    \item{object}{an object of class \code{"Fa"} or of a class derived from \code{"Fa"}}

    \item{...}{additional arguments, e.g., newdata: an optional data frame or matrix in which to look for variables with which to predict. If newdata is not missing, newdata should be scaled before \code{"predict"}.}

}

\section{Methods}{
\describe{

\item{\code{signature(object = "Fa")}}{
generic functions - see \code{show}, \code{print}, \code{summary}, \code{predict}, \code{plot}, \code{getCenter}, \code{getEigenvalues}, \code{getFa}, \code{getLoadings}, \code{getQuan}, \code{getScores}, \code{getSdev}
}

}}

\examples{
data(stock611)

## facovRegOgk is obtained from FaCov.default
## facovRegOgk uses the default method = "mle"
facovRegOgk=FaCov(x=scale(stock611[,3:12]), factors=3, cov.control = CovControlOgk(), 
scoresMethod = "regression"); facovRegOgk

## If missing newdata, the scores are used
predict(facovRegOgk)

## newdata must have the same center and scale attributes as
## the original data.
x=scale(stock611[,3:12])
newdata=stock611[1,3:12] # one row matrix
newdata=scale(newdata, center=attr(x,"scaled:center"), scale=attr(x,"scaled:scale"))
prediction=predict(facovRegOgk, newdata=newdata); prediction

}

\keyword{methods}
