\name{FaClassic}
\alias{FaClassic}
\alias{FaClassic.formula}
\alias{FaClassic.default}

\title{
Classical Factor Analysis
}
\description{
Performs a classical factor analysis and returns the results as an object of class \code{"FaClassic"} (a.k.a. constructor).
}
\usage{
FaClassic(x, ...)
\method{FaClassic}{formula}(formula, data = NULL, factors = 2, method = "mle", scoresMethod = "none", \dots)
\method{FaClassic}{default}(x, factors = 2, method = c("mle", "pca", "pfa"), 
scoresMethod = c("none", "regression", "Bartlett"), \dots)
}

\arguments{

  \item{x}{A formula or a numeric matrix or an object that can be coerced to a numeric matrix.}

  \item{\dots}{Arguments passed to or from other methods.}

  \item{formula}{A formula with no response variable, referring only to numeric variables.}

  \item{data}{An optional data frame (or similar: see \code{\link{model.frame}}) containing the variables in the \code{formula}.}

  \item{factors}{The number of factors to be fitted.}

  \item{method}{The method of factor analysis, one of "mle" (the default), "pca", and "pfa".}

  \item{scoresMethod}{Type of scores to produce, if any. The default is \code{"none"}, \code{"regression"} gives Thompson's scores, \code{"Bartlett"} gives Bartlett's weighted least-squares scores.}

}

%\details{}

\value{
An S4 object of class \code{\link{FaClassic-class}} which is a subclass of the virtual class \code{\link{Fa-class}}.    
}

\references{
Zhang, Y. Y. (2012), An Object Oriented Framework for Robust Factor Analysis.
}

\author{ Ying-Ying Zhang (Robert) \email{robertzhang@cqu.edu.cn} }

\seealso{
\code{\link{FaClassic-class}}, \code{\link{FaCov-class}}, \code{\link{FaRobust-class}}, \code{\link{Fa-class}}
}

\examples{
data(stock611)
stock604=stock611[-c(92,2,337,338,379,539,79),]

## faclassicReg604 uses the default method
faclassicReg604=FaClassic(x=scale(stock604[,3:12]), factors=3, 
      scoresMethod = "regression"); faclassicReg604

## faclassicForRegMle604 uses the formula interface
faclassicForRegMle604=FaClassic(~., data=as.data.frame(scale(stock604[,3:12])), factors=3, 
method = "mle", scoresMethod = "regression"); faclassicForRegMle604

}

\keyword{robust}
