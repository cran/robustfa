###################################################
###################################################
### The following are test codes, the  
### results are not reported in the paper!
###################################################
###################################################

## The code lines after "#### " are commented because
## they generate errors.

###################################################
### chunk number 1: 
###################################################
## set the prompt to "R> " and the continuation to "+ "
options(prompt = "R> ", continue = "+ ")


###################################################
### chunk number 2: intro
###################################################
##
## Load the 'robustfa' package and two data sets
## 
##
library("robustfa")
data("hbk")
hbk.x <- hbk[,1:3]              # take only the X part
 
data("stock611")
stock608 = stock611[-c(92,2,337),]
stock604 = stock611[-c(92,2,337,338,379,539,79),]
R611 = cor(stock611[,3:12]); R611

###################################################
### chunk number 3: plot-cov
###################################################
##
## setMethod("plot", signature(x = "CovRobust", y = "missing"), function(x, y = "missing",
##                                which = c("all", "dd", "distance", "qqchi2", "tolEllipsePlot", "screeplot"),
##                                classic = FALSE,
##                                ask = (which =  = "all" && dev.interactive(TRUE)),
##                                cutoff,
##                                id.n,
##                                tol = 1e-7, ...)
## 
## setMethod("plot", signature(x = "CovClassic", y = "missing"), function(x, y = "missing", 
##                                which = c("all", "distance", "qqchi2", "tolEllipsePlot", "screeplot"),
##                                ask = (which =  = "all" && dev.interactive(TRUE)),
##                                cutoff,
##                                id.n,
##                                tol = 1e-7, ...)
##
######################################################################## 
## The covariance matrix is singular! 
## However,the correlation matrix may not be singular!
## x = stock611[,3:12]

## control = "auto", "mcd", "ogk", "m", "mve", "sde", 
## "sfast", "surreal", "bisquare", "rocke" (these four are S-estimators)
covOgk = CovRobust(x = stock611[,3:12], control = "ogk"); covOgk
detail(covOgk)

## plot: Error in .local(x, y, ...) : The covariance matrix is singular!
#### dev.new(); plot(x = covOgk, which = "distance", classic = F)

## Error in solve.default(cov, ...) : 
##   system is computationally singular: reciprocal condition number = 1.28214e-21
#### covMcd = CovRobust(x = stock611[,3:12], control = "mcd"); covMcd

## Error in curve(expr = x, from = from, to = to, xlim = xlim, ylab = ylab,  : 
##  'expr' did not evaluate to an object of length 'n'
#### dev.new(); plot(x = covMcd, which = "distance", classic = F)

## the following 6 estimators don't work
#### covMcd = CovRobust(x = stock611[,3:12], control = "mcd"); covMcd
#### covM = CovRobust(x = stock611[,3:12], control = "m"); covM
#### covMve = CovRobust(x = stock611[,3:12], control = "mve"); covMve
#### covSde = CovRobust(x = stock611[,3:12], control = "sde"); covSde
## the following two estimators take so long, and are regarded as don't work
#### ## covSfast = CovRobust(x = stock611[,3:12], control = "sfast"); covSfast 
#### ## covSurreal = CovRobust(x = stock611[,3:12], control = "surreal"); covSurreal 

## "auto" equals "rocke" for x = stock611[,3:12]
## plot: Error in .local(x, y, ...) : The covariance matrix is singular!
covBisquare = CovRobust(x = stock611[,3:12], control = "bisquare"); covBisquare
#### dev.new(); plot(x = covBisquare, which = "distance", classic = F)
covRocke = CovRobust(x = stock611[,3:12], control = "rocke"); covRocke
#### dev.new(); plot(x = covRocke, which = "distance", classic = F)
covAuto = CovRobust(x = stock611[,3:12], control = "auto"); covAuto
#### dev.new(); plot(x = covAuto, which = "distance", classic = F)

## plot: Error in .local(x, y, ...) : The covariance matrix is singular!
covC = CovClassic(x = stock611[,3:12]); covC
#### dev.new(); plot(x = covC)

######################################################################## 
## x = stock604[,3:12]
## 7 stocks are identified as outliers. By 
## inspecting the entries of x6 find 3 stocks, and by scatter plot of the 
## first two principal components of the resulting 608 stocks find 4 stocks.
## See "Wang XM (2009). Applied Multivariate Analysis. 
## ShangHai University of Finance & Eco-nomics Press, Shanghai. 
## 3rd edition (This is a Chinese book)". The remaining 604 stocks are 
## regarded as good observations.

## control = "auto", "mcd", "ogk", "m", "mve", "sde", 
## "sfast", "surreal", "bisquare", "rocke" (these four are S-estimators)
covOgk = CovRobust(x = stock604[,3:12], control = "ogk"); covOgk
detail(covOgk)

## Error in .local(x, y, ...) : The covariance matrix is singular!
#### dev.new(); plot(x = covOgk, which = "distance", classic = F)

## the following 6 estimators don't work
#### covMcd = CovRobust(x = stock604[,3:12], control = "mcd"); covMcd
#### covM = CovRobust(x = stock604[,3:12], control = "m"); covM
#### covMve = CovRobust(x = stock604[,3:12], control = "mve"); covMve
#### covSde = CovRobust(x = stock604[,3:12], control = "sde"); covSde
## the following two estimators take so long, and are regarded as don't work
#### ## covSfast = CovRobust(x = stock604[,3:12], control = "sfast"); covSfast
#### ## covSurreal = CovRobust(x = stock604[,3:12], control = "surreal"); covSurreal

## "auto" equals "rocke" for x = stock604[,3:12]
## plot: Error in .local(x, y, ...) : The covariance matrix is singular!
covBisquare = CovRobust(x = stock604[,3:12], control = "bisquare"); covBisquare
#### dev.new(); plot(x = covBisquare, which = "distance", classic = F)
covRocke = CovRobust(x = stock604[,3:12], control = "rocke"); covRocke
#### dev.new(); plot(x = covRocke, which = "distance", classic = F)
covAuto = CovRobust(x = stock604[,3:12], control = "auto"); covAuto
#### dev.new(); plot(x = covAuto, which = "distance", classic = F)

## plot: Error in .local(x, y, ...) : The covariance matrix is singular!
covC = CovClassic(x = stock604[,3:12]); covC
#### dev.new(); plot(x = covC)

######################################################################## 
## Replace the "cov" slot of class "Cov" from covariance matrix to 
##     the correlation matrix. The correlation matrix is not singular!
## x = stock611[,3:12]

## control = "auto", "mcd", "ogk", "m", "mve", "sde", 
## "sfast", "surreal", "bisquare", "rocke" (these four are S-estimators)

## control = "ogk"
covOgk = CovRobust(x = stock611[,3:12], control = "ogk")
covOgk@cov = cov2cor(covOgk@cov); covOgk@cov
covOgk
detail(covOgk)
#### dev.new(); plot(x = covOgk, which = "dd") ## not OK

## Since "dev.new()" is available, "dev.copy(device = x11)" is no longer used.
## Because "dev.copy(device = x11)" will generates warnings like:
## In dev.copy(device = x11) : Font family not found in Windows font database.
##
## Only robust distances, no classical distances.
## Because CovC@cov corresponding to "classic = T" is singular!
dev.new(); plot(x = covOgk, which = "distance", classic = T)
#### dev.copy(device = x11)
#### savePlot(filename = "E:/research/my paper/Robust factor analysis/robustfa/inst/doc/cov_ogk_distance_classical", type = c("pdf"))

## Only robust distances, no classical distances.
dev.new(); plot(x = covOgk, which = "qqchi2", classic = T) 
#### savePlot(filename = "E:/research/my paper/Robust factor analysis/robustfa/inst/doc/cov_ogk_qqchi2_classical", type = c("pdf"))

## Warning message:
## In .local(x, y, ...) :
##   Warning: For tolerance ellipses the dimension must be 2!
#### dev.new(); plot(x = covOgk, which = "tolEllipsePlot", classic = T)

covOgk68 = CovRobust(x = stock611[,c(8,10)], control = "ogk")
dev.new(); plot(x = covOgk68, which = "tolEllipsePlot", classic = T)
#### savePlot(filename = "E:/research/my paper/Robust factor analysis/robustfa/inst/doc/cov_ogk_tolEllipsePlot_classical_cov6_8", type = c("pdf"))

## Replace the "cov" slot of covOgk68 from covariance matrix to 
## the correlation matrix. 
## The robust tolerance ellipes is tighter.
covOgk68@cov = cov2cor(covOgk68@cov); covOgk68@cov
dev.new(); plot(x = covOgk68, which = "tolEllipsePlot", classic = T)
#### savePlot(filename = "E:/research/my paper/Robust factor analysis/robustfa/inst/doc/cov_ogk_tolEllipsePlot_classical_cor6_8", type = c("pdf"))

## Note: covOgk@cov is the correlation matrix
##       covC@cov (covC is computed below) is the covariance matrix
dev.new(); plot(x = covOgk, which = "screeplot", classic = T)
#### savePlot(filename = "E:/research/my paper/Robust factor analysis/robustfa/inst/doc/cov_ogk_screeplot_classical", type = c("pdf"))
getEvals(covOgk)

## CovClassic
## covC1@cov is the covariance matrix
covC1 = CovClassic(x = stock611[,3:12]); covC1

## covC2@cov is the correlation matrix
## now covC2@cov = R611
covC2 = covC1
covC2@cov = cov2cor(covC1@cov); covC2@cov 
covC2
getEvals(covC1)
getEvals(covC2)
detail(covC2)

## Can not plot "tolEllipsePlot"?
dev.new(); plot(x = covC2) 

## the following don't work
#### covMcd = CovRobust(x = stock611[,3:12], control = "mcd"); covMcd
#### covM = CovRobust(x = stock611[,3:12], control = "m"); covM
#### covMve = CovRobust(x = stock611[,3:12], control = "mve"); covMve
#### covSde = CovRobust(x = stock611[,3:12], control = "sde"); covSde
## covSfast and covSurreal take so long, and they are regarded as don't work.
#### ## covSfast = CovRobust(x = stock611[,3:12], control = "sfast"); covSfast
#### ## covSurreal = CovRobust(x = stock611[,3:12], control = "surreal"); covSurreal

## the following work
covBisquare = CovRobust(x = stock611[,3:12], control = "bisquare"); covBisquare
covRocke = CovRobust(x = stock611[,3:12], control = "rocke"); covRocke
covAuto = CovRobust(x = stock611[,3:12], control = "auto"); covAuto

######################################################################## 
## control = "mcd"
## Note here: x = scale(stock611[,3:12])

covMcd = CovRobust(x = scale(stock611[,3:12]), control = "mcd"); covMcd

## all the following plots are OK
dev.new(); plot(x = covMcd, which = "dd")
#### savePlot(filename = "E:/research/my paper/Robust factor analysis/robustfa/inst/doc/cov_mcd_dd", type = c("pdf"))

## "myplotDD" is equivalent to "plot(x = covMcd, which = "dd")".
## "myplotDD" shows id.n and ind.
## Note: id.n and ind change each time due to covMcd changes each time!
## However, the ind of largest robust distances do not change.
dev.new(); myplotDD(x = covMcd)
#### savePlot(filename = "E:/research/my paper/Robust factor analysis/robustfa/inst/doc/cov_mcd_dd_my", type = c("pdf"))

dev.new(); plot(x = covMcd, which = "distance", classic = T)
#### savePlot(filename = "E:/research/my paper/Robust factor analysis/robustfa/inst/doc/cov_mcd_distance_classical", type = c("pdf"))

dev.new(); plot(x = covMcd, which = "qqchi2", classic = T)
#### savePlot(filename = "E:/research/my paper/Robust factor analysis/robustfa/inst/doc/cov_mcd_qqchi2_classical", type = c("pdf"))

## Warning message:
## In .local(x, y, ...) :
##   Warning: For tolerance ellipses the dimension must be 2!
#### dev.new(); plot(x = covMcd, which = "tolEllipsePlot", classic = T)

covMcd68 = CovRobust(x = stock611[,c(8,10)], control = "mcd")
dev.new(); plot(x = covMcd68, which = "tolEllipsePlot", classic = T)
#### savePlot(filename = "E:/research/my paper/Robust factor analysis/robustfa/inst/doc/cov_mcd_tolEllipsePlot_classical_cov6_8", type = c("pdf"))

## Replace the "cov" slot of covMcd68 from covariance matrix to 
## the correlation matrix. 
## The robust tolerance ellipes is tighter.
covMcd68@cov = cov2cor(covMcd68@cov); covMcd68@cov
dev.new(); plot(x = covMcd68, which = "tolEllipsePlot", classic = T)
#### savePlot(filename = "E:/research/my paper/Robust factor analysis/robustfa/inst/doc/cov_mcd_tolEllipsePlot_classical_cor6_8", type = c("pdf"))

dev.new(); plot(x = covMcd, which = "screeplot", classic = T)
#### savePlot(filename = "E:/research/my paper/Robust factor analysis/robustfa/inst/doc/cov_mcd_screeplot_classical", type = c("pdf"))

## all are OK:
## "mcd", "ogk", "m", "mve", "sde", 
## "sfast", "surreal", "bisquare", "rocke", "auto" (S-estimator)

covMcd = CovRobust(x = scale(stock611[,3:12]), control = "mcd"); covMcd
covOgk = CovRobust(x = scale(stock611[,3:12]), control = "ogk"); covOgk
covM = CovRobust(x = scale(stock611[,3:12]), control = "m"); covM
covMve = CovRobust(x = scale(stock611[,3:12]), control = "mve"); covMve
covSde = CovRobust(x = scale(stock611[,3:12]), control = "sde"); covSde
covSfast = CovRobust(x = scale(stock611[,3:12]), control = "sfast"); covSfast 
covSurreal = CovRobust(x = scale(stock611[,3:12]), control = "surreal"); covSurreal 
covBisquare = CovRobust(x = scale(stock611[,3:12]), control = "bisquare"); covBisquare 
covRocke = CovRobust(x = scale(stock611[,3:12]), control = "rocke"); covRocke 
covAuto = CovRobust(x = scale(stock611[,3:12]), control = "auto"); covAuto 

## covC = R611
## plot: can not plot "tolEllipsePlot"?
covC = CovClassic(x = scale(stock611[,3:12])); covC 
detail(covC)
dev.new(); plot(x = covC, which = "all") 

## covC68@cov is the covariance matrix 
covC68 = CovClassic(x = stock611[,c(8,10)])
dev.new(); plot(x = covC68, which = "tolEllipsePlot")
#### savePlot(filename = "E:/research/my paper/Robust factor analysis/robustfa/inst/doc/cov_tolEllipsePlot_classical_cov6_8", type = c("pdf"))

## covC68cor@cov is the correlation matrix
## The tolerance ellipse of covC68cor is tighter than that of covC68.
covC68cor = covC68
covC68cor@cov = cov2cor(covC68@cov); covC68cor@cov
dev.new(); plot(x = covC68cor, which = "tolEllipsePlot")
#### savePlot(filename = "E:/research/my paper/Robust factor analysis/robustfa/inst/doc/cov_tolEllipsePlot_classical_cor6_8", type = c("pdf"))

#############################################################################
## covOgk1@cov is the covariance matrix 
## covOgk2@cov is the correlation matrix 
covOgk1 = CovRobust(x = scale(stock611[,3:12]), control = "ogk"); covOgk1
covOgk2 = covOgk1
covOgk2@cov = cov2cor(covOgk1@cov); covOgk2@cov
covOgk2

## all the following plots are OK for x = covOgk1
dev.new(); plot(x = covOgk1, which = "dd")
#### savePlot(filename = "E:/research/my paper/Robust factor analysis/robustfa/inst/doc/cov_ogk1_dd", type = c("pdf"))

## show id.n and ind
dev.new(); myplotDD(x = covOgk1)
#### savePlot(filename = "E:/research/my paper/Robust factor analysis/robustfa/inst/doc/cov_ogk1_dd_my", type = c("pdf"))

dev.new(); plot(x = covOgk1, which = "distance", classic = T)
#### savePlot(filename = "E:/research/my paper/Robust factor analysis/robustfa/inst/doc/cov_ogk1_distance_classical", type = c("pdf"))

dev.new(); plot(x = covOgk1, which = "qqchi2", classic = T)
#### savePlot(filename = "E:/research/my paper/Robust factor analysis/robustfa/inst/doc/cov_ogk1_qqchi2_classical", type = c("pdf"))

## Warning message:
## In .local(x, y, ...) :
##   Warning: For tolerance ellipses the dimension must be 2!
#### dev.new(); plot(x = covOgk1, which = "tolEllipsePlot", classic = T)

covOgk168 = CovRobust(x = scale(stock611[,c(8,10)]), control = "ogk")
dev.new(); plot(x = covOgk168, which = "tolEllipsePlot", classic = T)
#### savePlot(filename = "E:/research/my paper/Robust factor analysis/robustfa/inst/doc/cov_ogk1_tolEllipsePlot_classical_6_8", type = c("pdf"))

dev.new(); plot(x = covOgk1, which = "screeplot", classic = T)
#### savePlot(filename = "E:/research/my paper/Robust factor analysis/robustfa/inst/doc/cov_ogk1_screeplot_classical", type = c("pdf"))

## all the following plots are OK for x = covOgk2
dev.new(); plot(x = covOgk2, which = "dd")
#### savePlot(filename = "E:/research/my paper/Robust factor analysis/robustfa/inst/doc/cov_ogk2_dd", type = c("pdf"))

## show id.n and ind
myplotDD(x = covOgk2)
#### savePlot(filename = "E:/research/my paper/Robust factor analysis/robustfa/inst/doc/cov_ogk2_dd_my", type = c("pdf"))

dev.new(); plot(x = covOgk2, which = "distance", classic = T)
#### savePlot(filename = "E:/research/my paper/Robust factor analysis/robustfa/inst/doc/cov_ogk2_distance_classical", type = c("pdf"))

dev.new(); plot(x = covOgk2, which = "qqchi2", classic = T)
#### savePlot(filename = "E:/research/my paper/Robust factor analysis/robustfa/inst/doc/cov_ogk2_qqchi2_classical", type = c("pdf"))

## Warning message:
## In .local(x, y, ...) :
##   Warning: For tolerance ellipses the dimension must be 2!
#### dev.new(); plot(x = covOgk2, which = "tolEllipsePlot", classic = T)

covOgk268 = CovRobust(x = scale(stock611[,c(8,10)]), control = "ogk")
covOgk268@cov = cov2cor(covOgk268@cov); covOgk268@cov
dev.new(); plot(x = covOgk268, which = "tolEllipsePlot", classic = T)
#### savePlot(filename = "E:/research/my paper/Robust factor analysis/robustfa/inst/doc/cov_ogk2_tolEllipsePlot_classical_6_8", type = c("pdf"))

dev.new(); plot(x = covOgk2, which = "screeplot", classic = T)
#### savePlot(filename = "E:/research/my paper/Robust factor analysis/robustfa/inst/doc/cov_ogk2_screeplot_classical", type = c("pdf"))

## Summary results of covOgk1 and covOgk2
## 
##                                   covOgk1                covOgk2
## ------------------------------------------------------------------------
##                          @cov |   covariance            correlation
##              Robust distances |     big                   small (close to classical)
## The first several eigenvalues |    small                   big (close to classical)
##      Robust tolerance ellipes |    small                   big (close to classical)
##
## From the above results, we favor covOgk1.

###################################################
### chunk number 4: FaCov
###################################################
##
## x = stock611[,3:12]
##     OK: CovControlOgk
## not OK: CovControlMcd, CovControlMest, CovControlMve, CovControlSest, CovControlSde

## cov.control = CovControlOgk() OK
## the following FaCov uses the default method
facovRegOgk = FaCov(x = stock611[,3:12], factors = 3, cov.control = CovControlOgk(), 
      scoresMethod = "regression"); facovRegOgk 
summary(facovRegOgk)
detail(facovRegOgk)

## the following 5 estimators don't work
## Error in solve.default(cov, ...) : 
##   system is computationally singular: reciprocal condition number = 1.52295e-21
#### facovRegMcd = FaCov(x = stock611[,3:12], factors = 3, cov.control = CovControlMcd(), 
####       scoresMethod = "regression"); facovRegMcd
#### facovRegM = FaCov(x = stock611[,3:12], factors = 3, cov.control = CovControlMest(), 
####       scoresMethod = "regression"); facovRegM
#### facovRegMve = FaCov(x = stock611[,3:12], factors = 3, cov.control = CovControlMve(), 
####       scoresMethod = "regression"); facovRegMve
## facovRegS takes so long a time, and it is regarded as don't work.
#### ## facovRegS = FaCov(x = stock611[,3:12], factors = 3, cov.control = CovControlSest(), 
#### ##       scoresMethod = "regression"); facovRegS
#### facovRegSde = FaCov(x = stock611[,3:12], factors = 3, cov.control = CovControlSde(), 
####       scoresMethod = "regression"); facovRegSde 

#############################################################################
## x = scale(stock611[,3:12])
## all are OK: 
## CovControlOgk, CovControlMcd, CovControlMest, CovControlMve, 
## CovControlSde, CovControlSest

## cov.control = CovControlOgk() OK
facovRegOgk = FaCov(x = scale(stock611[,3:12]), factors = 3, cov.control = CovControlOgk(), 
      scoresMethod = "regression"); facovRegOgk
summary(facovRegOgk)
detail(facovRegOgk)

facovRegMcd = FaCov(x = scale(stock611[,3:12]), factors = 3, cov.control = CovControlMcd(), 
      scoresMethod = "regression"); facovRegMcd
summary(facovRegMcd)
detail(facovRegMcd)

facovRegM = FaCov(x = scale(stock611[,3:12]), factors = 3, cov.control = CovControlMest(), 
      scoresMethod = "regression"); facovRegM
facovRegMve = FaCov(x = scale(stock611[,3:12]), factors = 3, cov.control = CovControlMve(), 
      scoresMethod = "regression"); facovRegMve
facovRegS = FaCov(x = scale(stock611[,3:12]), factors = 3, cov.control = CovControlSest(), 
      scoresMethod = "regression"); facovRegS 
facovRegSde = FaCov(x = scale(stock611[,3:12]), factors = 3, cov.control = CovControlSde(), 
      scoresMethod = "regression"); facovRegSde

#############################################################################
## FaCov with cov.control = NULL 

## x = scale(stock611[,3:12]) not OK
## Error in factanal(factors = factors, covmat = covmat) : 
##  unable to optimize from these starting value(s)
#### facovRegNullScale611 = FaCov(x = scale(stock611[,3:12]), factors = 3, cov.control = NULL, 
####       scoresMethod = "regression"); facovRegNullScale611

## x = scale(stock604[,3:12]) OK
facovRegNullScale604 = FaCov(x = scale(stock604[,3:12]), factors = 3, cov.control = NULL, 
      scoresMethod = "regression"); facovRegNullScale604

## x = stock611[,3:12] not OK
## Error in factanal(factors = factors, covmat = covmat) : 
##  unable to optimize from these starting value(s)
#### facovRegNull611 = FaCov(x = stock611[,3:12], factors = 3, cov.control = NULL, 
####       scoresMethod = "regression"); facovRegNull611

## x = stock604[,3:12] OK
## facovRegNull604 = facovRegNullScale604
facovRegNull604 = FaCov(x = stock604[,3:12], factors = 3, cov.control = NULL, 
      scoresMethod = "regression"); facovRegNull604

###################################################
### chunk number 5: plot-Fa
###################################################
##
## setMethod("plot", "Fa", function(x, which = c("factorScore", "screeplot"),
##                                  choices = 1:2, ...)
##
## x      : an object of class "Fa"
## which  : indicate what kind of plot 
##          "factorScore": the scatterplot of the factor scores
##          "screeplot": shows the eigenvalues and is helpful to select
##                       the number of factors.
## choices: an integer vector indicate which columns of the factor scores to plot
## 
##########################################################################
## FaClassic

## For x = scale(stock611[,3:12]), error
## Error in factanal(factors = factors, covmat = covmat) : 
##  unable to optimize from these starting value(s)
#### facovRegNull = FaCov(x = scale(stock611[,3:12]), factors = 3, cov.control = NULL, 
####       scoresMethod = "regression"); facovRegNull

## So we turn to x = scale(stock604[,3:12]), and it works
facovRegNull = FaCov(x = scale(stock604[,3:12]), factors = 3, cov.control = NULL, 
      scoresMethod = "regression"); facovRegNull

## The above computation is equivalent to the following computation using "FaClassic"
## note: the default method is "mle"
## faclassicReg604 = facovRegNull
faclassicReg604 = FaClassic(x = scale(stock604[,3:12]), factors = 3, 
      scoresMethod = "regression"); faclassicReg604

## In fact, faclassicReg604 uses "FaClassic.default". We can use the
## formula interface instead, that is "FaClassic.formula".
## faclassicForRegMle604 = faclassicReg604 = facovRegNull
faclassicForRegMle604 = FaClassic(~., data = as.data.frame(scale(stock604[,3:12])), factors = 3, 
method = "mle", scoresMethod = "regression"); faclassicForRegMle604

dev.new(); plot(faclassicReg604, which = "factorScore", choices = 1:2)
#### savePlot(filename = "E:/research/my paper/Robust factor analysis/robustfa/inst/doc/factorScore_regression_classical_604", type = c("pdf"))

dev.new(); plot(faclassicReg604, which = "screeplot")
#### savePlot(filename = "E:/research/my paper/Robust factor analysis/robustfa/inst/doc/screeplot_regression_classical_604", type = c("pdf"))

## Turn back to stock611[,3:12], we use another method starting from x = scale(stock604[,3:12])
x = scale(stock604[,3:12]); 
y = scale(stock611[,3:12], center = attr(x,"scaled:center"), scale = attr(x,"scaled:scale")); 
factors = 3; scoresMethod = "regression"
    
data <- as.matrix(x)
n <- nrow(data)
p <- ncol(data)

## use Cov (or CovClassic) to calculate the covariance matrix
covx <- Cov(data)
covmat <- list(cov = getCov(covx), center = getCenter(covx), n.obs = covx@n.obs)

out <- factanal(factors = factors, covmat = covmat)
## note here
scores <- as.matrix(y) %*% solve(out$correlation) %*% out$loadings[]
faclassicReg611 <- new("FaClassic", call = out$call,
            converged = out$converged,
            loadings = out$loadings[], # class(out$loadings) =  = "loadings", class(out$loadings[]) =  = "matrix"
            uniquenesses = out$uniquenesses,
            correlation = out$correlation,
            criteria = out$criteria,
            factors = out$factors,
            dof = out$dof,
            method = out$method,
            scores = scores,
            scoresMethod = scoresMethod,
            STATISTIC = out$STATISTIC,
            PVAL = out$PVAL,
            n.obs = nrow(y),
            center = getCenter(covx),
            eigenvalues = eigen(out$correlation)$values,
            cov.control = NULL)
faclassicReg611
detail(faclassicReg611)

dev.new(); plot(faclassicReg611, which = "factorScore", choices = 1:2)
#### savePlot(filename = "E:/research/my paper/Robust factor analysis/robustfa/inst/doc/factorScore_regression_classical_611", type = c("pdf"))

dev.new(); plot(faclassicReg611, which = "screeplot")
#### savePlot(filename = "E:/research/my paper/Robust factor analysis/robustfa/inst/doc/screeplot_regression_classical_611", type = c("pdf"))

## In fact, we can test that the scores of faclassicReg604 (604 observations)
## and the corresponding scores of faclassicReg611 (604 observations) are equal.
faclassicReg604@scores-faclassicReg611@scores[-c(92,2,337,338,379,539,79),]
(faclassicReg604@scores-faclassicReg611@scores[-c(92,2,337,338,379,539,79),])/faclassicReg604@scores

## FaClassic, x = scale(stock611[,3:12]), 
## method = "mle" error
## Error in factanal(factors = factors, covmat = covmat) : 
##   unable to optimize from these starting value(s)
#### faclassicReg611 = FaClassic(x = scale(stock611[,3:12]), factors = 3, 
####       scoresMethod = "regression"); faclassicReg611

## method = "pca" OK
faclassicRegPca611 = FaClassic(x = scale(stock611[,3:12]), factors = 2, method = "pca",
      scoresMethod = "regression"); faclassicRegPca611
summary(faclassicRegPca611)
dev.new(); plot(faclassicRegPca611, which = "factorScore", choices = 2:1)
#### savePlot(filename = "E:/research/my paper/Robust factor analysis/robustfa/inst/doc/FS_reg_c_611", type = c("pdf"))

dev.new(); plot(faclassicRegPca611, which = "screeplot")
#### savePlot(filename = "E:/research/my paper/Robust factor analysis/robustfa/inst/doc/scree_reg_c_611", type = c("pdf"))

############################################################################
## FaCov, x = scale(stock611[,3:12])
## All are OK: 
## CovControlOgk, CovControlMcd, CovControlMest, 
## CovControlMve, CovControlSde, CovControlSest

## FaCov with cov.control = CovControlOgk(), method = "mle"
## facovRegOgk is obtained from FaCov.default
## facovRegOgk uses the default method = "mle"
facovRegOgk = FaCov(x = scale(stock611[,3:12]), factors = 3, cov.control = CovControlOgk(), 
      scoresMethod = "regression"); facovRegOgk
summary(facovRegOgk)

## In fact, it is equivalent to use FaCov.formula
## facovForRegOgkMle = facovRegOgk
facovForRegOgkMle = FaCov(~., data = as.data.frame(scale(stock611[,3:12])), factors = 3, cov.control = CovControlOgk(),
method = "mle", scoresMethod = "regression"); facovForRegOgkMle
summary(facovForRegOgkMle)

## For ogk:
## F1 explains x3, x4, x5, x7, x8
## F2 explains x1, x2, x3, x4, x9, x10
## facovRegOgk uses the default method = "mle"

dev.new(); plot(facovRegOgk, which = "factorScore", choices = 1:2)
#### savePlot(filename = "E:/research/my paper/Robust factor analysis/robustfa/inst/doc/factorScore_regression_robust_ogk", type = c("pdf"))

dev.new(); plot(facovRegOgk, which = "screeplot")
#### savePlot(filename = "E:/research/my paper/Robust factor analysis/robustfa/inst/doc/screeplot_regression_robust_ogk", type = c("pdf"))

###########################################################################
## FaCov with cov.control = CovControlOgk(), method = "pca"

facovRegOgkPca = FaCov(x = scale(stock611[,3:12]), factors = 3, cov.control = CovControlOgk(), 
                    method = "pca", scoresMethod = "regression"); facovRegOgkPca
summary(facovRegOgkPca)

dev.new(); plot(facovRegOgkPca, which = "factorScore", choices = 1:2)
#### savePlot(filename = "E:/research/my paper/Robust factor analysis/robustfa/inst/doc/factorScore_regression_robust_ogk_pca", type = c("pdf"))

dev.new(); plot(facovRegOgkPca, which = "screeplot")
#### savePlot(filename = "E:/research/my paper/Robust factor analysis/robustfa/inst/doc/screeplot_regression_robust_ogk_pca", type = c("pdf"))

###########################################################################
## FaCov with cov.control = CovControlOgk(), method = "pfa"

facovRegOgkPfa = FaCov(x = scale(stock611[,3:12]), factors = 3, cov.control = CovControlOgk(), 
                    method = "pfa", scoresMethod = "regression"); facovRegOgkPfa
summary(facovRegOgkPfa)

dev.new(); plot(facovRegOgkPfa, which = "factorScore", choices = 1:2)
#### savePlot(filename = "E:/research/my paper/Robust factor analysis/robustfa/inst/doc/factorScore_regression_robust_ogk_pfa", type = c("pdf"))

dev.new(); plot(facovRegOgkPfa, which = "screeplot")
#### savePlot(filename = "E:/research/my paper/Robust factor analysis/robustfa/inst/doc/screeplot_regression_robust_ogk_pfa", type = c("pdf"))

###########################################################################
## FaCov with cov.control = CovControlMcd(), method = "mle"

facovRegMcd = FaCov(x = scale(stock611[,3:12]), factors = 3, cov.control = CovControlMcd(), 
      scoresMethod = "regression"); facovRegMcd
summary(facovRegMcd)
## For mcd:
## F1 explains x1, x2, x3, x4, x9, x10 
## F2 explains x3, x4, x5, x7, x8
## So mcd_F1 is similar to ogk_F2 and mcd_F2 is similar to ogk_F1 !

dev.new(); plot(facovRegMcd, which = "factorScore", choices = c(2,1))
#### savePlot(filename = "E:/research/my paper/Robust factor analysis/robustfa/inst/doc/factorScore_regression_robust_mcd", type = c("pdf"))

dev.new(); plot(facovRegMcd, which = "screeplot")
#### savePlot(filename = "E:/research/my paper/Robust factor analysis/robustfa/inst/doc/screeplot_regression_robust_mcd", type = c("pdf"))


