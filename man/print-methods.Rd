\name{print-methods}
\docType{methods}
\alias{print-methods}
\alias{print,Fa-method}
\alias{print,SummaryFa-method}
\alias{print}

\title{Print/Display an Object}

\description{
Print/display an object, including the Call, Standard deviations, Loadings.
}

\usage{
print(x, ...)
} 

\arguments{

    \item{x}{
an object of class \code{"Fa"} or \code{"SummaryFa"} or of a class derived from \code{"Fa"} or of class or \code{"SummaryFa"}. 
}

    \item{...}{
additional arguments, e.g., print.x=TRUE
}

}

\section{Methods}{
\describe{

\item{x = "Fa"}{
generic functions - see \code{print}, \code{summary}, \code{predict}, \code{plot}, \code{getCenter}, \code{getEigenvalues}, \code{getFa}, \code{getLoadings}, \code{getQuan}, \code{getScores}, \code{getSdev}
}

\item{x = "SummaryFa"}{
generic functions - see \code{print}, \code{summary}, \code{predict}, \code{plot}, \code{getCenter}, \code{getEigenvalues}, \code{getFa}, \code{getLoadings}, \code{getQuan}, \code{getScores}, \code{getSdev}
}
}}

\value{
An invisible argument \code{x}.
}

\references{
Zhang, Y. Y. (2013), An Object Oriented Solution for Robust Factor Analysis.
}

\author{ Ying-Ying Zhang (Robert) \email{robertzhangyying@qq.com} }

\seealso{\code{\link{Fa-class}}, \code{\link{SummaryFa-class}}}

\examples{

data("hbk")
hbk.x = hbk[,1:3] 

faCovPcaRegMcd = FaCov(x = hbk.x, factors = 2, method = "pca",
scoresMethod = "regression", cov.control = rrcov::CovControlMcd())

## you can use either object or print(object) or myFaPrint(object)
## since faCovPcaRegMcd is an object of class "Fa"

faCovPcaRegMcd
print(faCovPcaRegMcd)
myFaPrint(faCovPcaRegMcd)

}

\keyword{methods}
