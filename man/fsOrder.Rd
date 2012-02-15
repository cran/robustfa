\name{fsOrder}
\alias{fsOrder}

\title{
Compute the Ordered Factor Scores
}
\description{
Compute the ordered factor scores according to the first/second/third... column of the original factor scores.
}
\usage{
fsOrder(factorScores)
}

\arguments{
  \item{factorScores}{
The original factor scores.
}
}
%% \details{}

\value{
A list with \code{m} (the number of factors) components:

\item{[[1]] }{The ordered factor scores with a decreasing first column.}

\item{[[2]] }{The ordered factor scores with a decreasing second column.}

\item{... }{}

\item{[[m]] }{The ordered factor scores with a decreasing m-th column.}

}
\references{
Zhang, Y. Y. (2012), An Object Oriented Framework for Robust Factor Analysis.
}

\author{ Ying-Ying Zhang (Robert) \email{robertzhang@cqu.edu.cn} }

\seealso{
\code{\link{order}}
}

\examples{
data(stock611)
R611=cor(stock611[,3:12]); R611

## FS.pca contains scores etc.
fsPca=factorScorePca(x = stock611[,3:12], factors = 2, rotation = "varimax", 
scoresMethod = "regression"); fsPca

orderedFS=fsOrder(fsPca$scores); orderedFS
}

\keyword{robust}
