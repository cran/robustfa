\name{factorScorePca}
\alias{factorScorePca}

\title{
Factor Analysis by Principal Component Analysis (PCA)
}
\description{
Perform principal component factor analysis on a covariance matrix or data matrix. 
}
\usage{
factorScorePca(x, factors = 2, covmat = NULL, rotation = c("varimax", "none"), 
scoresMethod = c("none", "regression", "Bartlett"))
}

\arguments{
  \item{x}{
A numeric matrix or an object that can be coerced to a numeric matrix.
}
  \item{factors}{
The number of factors to be fitted.
}
  \item{covmat}{
A covariance matrix, or a covariance list as returned by \code{\link{cov.wt}}. Of course, correlation matrices are covariance matrices.
}
  \item{rotation}{
character. "none" or "varimax": it will be called with first argument the loadings matrix, and should return a list with component \code{loadings} giving the rotated loadings, or just the rotated loadings.
}
  \item{scoresMethod}{
Type of scores to produce, if any. The default is \code{"none"}, \code{"regression"} gives Thompson's scores, \code{"Bartlett"} gives Bartlett's weighted least-squares scores.
}
}

\details{
"factorScorePca" always uses the correlation matrix. A covariance matrice will be scaled into the corresponding correlation matrix efficiently by \code{\link{cov2cor}}.

Other feasible usages are:
\code{factorScorePca(factors, covmat)}
\code{factorScorePca(x, factors, rotation, scoresMethod)}

If \code{x} is missing, then the following components of the result will be NULL: scores, ScoringCoef, mean.F, cor.F, n.obs, and center.
}

\value{
An object of class \code{"factorScorePca"} with components:

\item{call }{The matched call.}

\item{loadings }{A matrix of loadings, one column for each factor. This is of class \code{"loadings"} if \code{rotation = "varimax"}: see \code{\link{loadings}} for its print method; It is a plain matrix if \code{rotation = "none"}.}

\item{communality }{The common variance.}

\item{uniquenesses }{The uniquenesses/specific variance computed.}

\item{correlation }{The correlation matrix used.}

\item{factors }{The argument factors.}

\item{method }{The method: always \code{"pca"}.}

\item{scores }{If requested, a matrix of scores. NULL if \code{x} is missing.}

\item{scoringCoef }{The scoring coefficients. NULL if \code{x} is missing.}

\item{meanF }{The sample mean of the scores. NULL if \code{x} is missing.}

\item{corF }{The sample correlation matrix of the scores. NULL if \code{x} is missing.}

\item{scoresMethod }{The argument \code{scoresMethod}.}

\item{n.obs }{The number of observations if available. NULL if \code{x} is missing.}

\item{center }{The center of the data. NULL if \code{x} is missing.}

\item{eigenvalues }{The eigenvalues of the correlation matrix.}

}

\references{
Zhang, Y. Y. (2012), An Object Oriented Framework for Robust Factor Analysis.
}

\author{ Ying-Ying Zhang (Robert) \email{robertzhang@cqu.edu.cn} }

\seealso{
\code{\link{factorScorePfa}}, \code{\link{factanal}}
}

\examples{
data(stock611)
R611=cor(stock611[,3:12]); R611

## covmat is a matrix
fsPca1=factorScorePca(factors = 3, covmat = R611); fsPca1

## covmat is a list
covx <- Cov(stock611[,3:12])
covmat <- list(cov=getCov(covx), center=getCenter(covx), n.obs=covx@n.obs)
fsPca2=factorScorePca(factors = 3, covmat = covmat); fsPca2

## fsPca3 contains scores etc.
fsPca3=factorScorePca(x = stock611[,3:12], factors = 2, rotation = "varimax", 
scoresMethod = "regression"); fsPca3
}

\keyword{robust}
