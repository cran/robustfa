\name{computeScores}
\alias{computeScores}

\title{
Compute Factor Scores
}
\description{
Compute factor scores on the result of factor analysis method, the method is one of "mle", "pca", and "pfa". Prediction is the factor scores.
}
\usage{
computeScores(x, newdata, scoresMethod)
}

\arguments{
  \item{x}{
A list with components: correlation, loadings, and uniquenesses.
}
  \item{newdata}{
A matrix on which to predict.
}
  \item{scoresMethod}{
Type of scores to produce, if any. The default is \code{"none"}, \code{"regression"} gives Thompson's scores, \code{"Bartlett"} gives Bartlett's weighted least-squares scores.
}
}
\details{
\code{x} must be a list with components: correlation, loadings, and uniquenesses. Otherwise returns an error "x is not a list".
}
\value{
A matrix of factor scores. NULL if \code{scoresMethod = "none"}.
}

\references{
Zhang, Y. Y. (2012), An Object Oriented Framework for Robust Factor Analysis.
}

\author{ Ying-Ying Zhang (Robert) \email{robertzhang@cqu.edu.cn} }

%% \seealso{}

\examples{
data(stock611)
data=scale(stock611[,3:12])

covx <- restimate(obj=CovControlOgk(), x=data) ## note the names: obj, x
covmat <- list(cov=getCov(covx), center=getCenter(covx), n.obs=covx@n.obs)
out=factanal(factors=3, covmat=covmat)

scores <- computeScores(out, newdata=scale(data), scoresMethod="regression"); scores

}

\keyword{robust}
