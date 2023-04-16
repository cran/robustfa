\name{robustfa-package}
\alias{robustfa-package}
\alias{robustfa}
\docType{package}
\title{
An Object Oriented Solution for Robust Factor Analysis
}
\description{
Outliers virtually exist in any datasets of any application field. To avoid the impact of outliers, we need to use robust estimators. Classical estimators of multivariate mean and covariance matrix are the sample mean and the sample covariance matrix. Outliers will affect the sample mean and the sample covariance matrix, and thus they will affect the classical factor analysis which depends on the classical estimators (Pison, G., Rousseeuw, P.J., Filzmoser, P. and Croux, C. (2003) \doi{10.1016/S0047-259X(02)00007-6}). So it is necessary to use the robust estimators of the sample mean and the sample covariance matrix. There are several robust estimators in the literature: MCD, OGK, MVE, M, S, and Stahel-Donoho. The most direct way to robustify multivariate anal-ysis is to replace the sample mean and the sample covariance matrix of the classical estimators to robust estima-tors (Maronna, R.A., Martin, D. and Yohai, V. (2006) \doi{10.1002/0470010940}) (Todorov, V. and Filzmoser, P. (2009) \doi{10.18637/jss.v032.i03}), which is our choice of robust factor analysis. robustfa is an object oriented solution for robust factor analysis. In the solution, new S4 classes \code{"Fa"}, \code{"FaClassic"}, \code{"FaRobust"}, \code{"FaCov"}, \code{"SummaryFa"} are created.
}
\details{
\tabular{ll}{
Package: \tab robustfa\cr
Type: \tab Package\cr
Version: \tab 1.0-5\cr
Date: \tab 2013-11-09\cr
License: \tab GPL (>= 2)\cr
Depends: \tab methods\cr
}
The most important functions are:
\code{\link{FaClassic}}, \code{\link{FaCov}}, \code{\link{factorScorePca}}, \code{\link{factorScorePfa}}
}
\author{
Ying-Ying Zhang (Robert)

Maintainer: Ying-Ying Zhang (Robert) <robertzhangyying@qq.com>
}
\references{
Zhang, Y. Y. (2013), An Object Oriented Solution for Robust Factor Analysis.
}
\keyword{ package }
%% \seealso{}
\examples{
library("robustfa")

}
