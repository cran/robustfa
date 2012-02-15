\name{summary-methods}
\docType{methods}
\alias{summary-methods}
\alias{summary,Fa-method}
\alias{summary}

\title{Summary an Object}

\description{
Produce result summaries of an object of class "Fa". 
}

\usage{
summary(object, ...)
} 

\arguments{

    \item{object}{an object of class \code{"Fa"} or of a class derived from \code{"Fa"}.
}

    \item{...}{
additional arguments, e.g., print.x=TRUE.
}

}

\section{Methods}{
\describe{

\item{\code{signature(object = "Fa")}}{
Summary an object of class "Fa".
}

}}

\examples{
data(stock611)

facovRegOgk=FaCov(x=scale(stock611[,3:12]), factors=3, cov.control = CovControlOgk(), 
scoresMethod = "regression")

## summaryFacovRegOgk is an object of class "SummaryFa"
summaryFacovRegOgk=summary(facovRegOgk); summaryFacovRegOgk

}

\keyword{methods}

