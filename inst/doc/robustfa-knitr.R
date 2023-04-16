## ----include=FALSE------------------------------------------------------------
library(knitr)
opts_chunk$set(
fig.path='robustfa'
)

## ----label=set-prompt2, echo=FALSE, results='hide'------------------
  ##
  ## set the prompt to "R> " and the continuation to "+ "
  ##
options(prompt = "R> ", continue = "+  ", width = 70, useFancyQuotes = FALSE)

## ----label=library-hbk-stock611, echo=FALSE, results='hide'---------
  ##
  ## Load the 'robustfa' package and two data sets
  ##
library("robustfa")
data("hbk")
hbk.x = hbk[,1:3] # take only the X part
rownames(hbk.x) = 1:75

data("stock611")
stock611$name = 1:611; stock611
stock608 = stock611[-c(92,2,337),]
stock604 = stock611[-c(92,2,337,338,379,539,79),]
R611 = cor(stock611[,3:12]); R611

## ----label=FaCov.default, results='hide'----------------------------
  ##
  ## faCovPcaRegMcd is obtained from FaCov.default
  ##
  faCovPcaRegMcd = FaCov(x = hbk.x, factors = 2, method = "pca",
                         scoresMethod = "regression", cov.control = rrcov::CovControlMcd());
faCovPcaRegMcd

## ----label=show-print-summary_Fa, echo=FALSE, results='hide'--------
faCovPcaRegMcd
summary(faCovPcaRegMcd)

## ----label=FaCov.formula, results='hide'----------------------------
  faCovForPcaRegMcd = FaCov(~., data = as.data.frame(hbk.x),
                            factors = 2, method = "pca", scoresMethod = "regression",
                            cov.control = rrcov::CovControlMcd())

## ----label=show-print-summary_Fa2, echo=FALSE, results='hide'-------
  faCovForPcaRegMcd
summary(faCovForPcaRegMcd)

## ----label=show-print-summary_Fa3-----------------------------------
  class(faCovPcaRegMcd)

## ----label=show-print-summary_Fa4-----------------------------------
  print(faCovPcaRegMcd)

## ----label=show-print-summary_Fa5, echo=FALSE, results='hide'-------
  faCovPcaRegMcd
myFaPrint(faCovPcaRegMcd)

## ----label=show-print-summary_Fa6-----------------------------------
  summaryFaCovPcaRegMcd = summary(faCovPcaRegMcd);
summaryFaCovPcaRegMcd

## ----label=show-print-summary_Fa7, echo=FALSE, results='hide'-------
  print(summaryFaCovPcaRegMcd)
class(summaryFaCovPcaRegMcd)

## ----label=predict_Fa, results='hide'-------------------------------
  predict(faCovPcaRegMcd)

## ----label=predict_Fa2, echo=FALSE----------------------------------
  predict(faCovPcaRegMcd)[c(1:5, 71:75),]

## ----label=predict_Fa3, results='hide'------------------------------
  newdata = hbk.x[1, ]
cor = FALSE # the default
newdata = { if (cor == TRUE)
  # standardized transformation
  scale(newdata, center = faCovPcaRegMcd@center,
        scale = sqrt(diag(faCovPcaRegMcd@covariance)))
  else # cor == FALSE
    # centralized transformation
    scale(newdata, center = faCovPcaRegMcd@center, scale = FALSE)
}

## ----label=predict_Fa4----------------------------------------------
  prediction = predict(faCovPcaRegMcd, newdata = newdata)
prediction

## ----label=plot-Fa-FaClassic-FaCov----------------------------------
  faClassicPcaReg = FaClassic(x = hbk.x, factors = 2, method = "pca",
                              scoresMethod = "regression"); faClassicPcaReg
summary(faClassicPcaReg)

## ----label=plot-Fa-FaClassic-FaCov2, echo=FALSE, results='hide'-----
  ##
  ## FaCov
  ##
  faCovPcaRegMcd = FaCov(x = hbk.x, factors = 2, method = "pca",
                         scoresMethod = "regression", cov.control = rrcov::CovControlMcd()); faCovPcaRegMcd
summary(faCovPcaRegMcd)

## ----label=hbk_factorScore, echo=TRUE, results='hide', include=FALSE----
usr <- par(mfrow=c(1,2))
plot(faClassicPcaReg, which = "factorScore", choices = 1:2)
plot(faCovPcaRegMcd, which = "factorScore", choices = 1:2)
par(usr)

## ----label=hbk_screeplot, echo=TRUE, results='hide', include=FALSE----
usr <- par(mfrow=c(1,2))
plot(faClassicPcaReg, which = "screeplot")
plot(faCovPcaRegMcd, which = "screeplot")
par(usr)

## ----label=hbk_vs_classical_robust, echo=FALSE, results='hide', include=FALSE----
  ##
  ## Plot of the first two factors of hbk and
  ## 97.5% tolerance ellipse plot: classical and robust.
  ##
cfaClassicPcaReg <- list(center = c(0,0), cov = diag(faClassicPcaReg@eigenvalues[1:2]), n.obs = faClassicPcaReg@n.obs)
cfaCovPcaRegMcd <- list(center = c(0,0), cov = diag(faCovPcaRegMcd@eigenvalues[1:2]), n.obs = faCovPcaRegMcd@n.obs)

usr <- par(mfrow=c(1,2))
rrcov:::.myellipse(faClassicPcaReg@scores, xcov = cfaClassicPcaReg,
                   main = "Classical, method = \"pca\"", xlab = "Factor1", ylab = "Factor2", id.n = 0,
                   xlim = c(-40,40), ylim = c(-5,15))
abline(v = 0)
abline(h = 0)
text(5,0,labels = "1-13", cex = 0.8)
text(0.5,6,labels = "14", cex = 0.8)
rrcov:::.myellipse(faCovPcaRegMcd@scores, xcov = cfaCovPcaRegMcd,
                   main = "Robust (MCD), method = \"pca\"", xlab = "Factor1", ylab = "Factor2", id.n = 4,
                   xlim = c(-40,40), ylim = c(-5,15))
text(22,9.5,labels = "1-10", cex = 0.8)
abline(v = 0)
abline(h = 0)
par(usr)

## xlim = c(-40,40), ylim = c(-5,15) # the first ellipse is large
## xlim = c(-2,32), ylim = c(-2,14)
apply(rbind(faClassicPcaReg@scores, faCovPcaRegMcd@scores), 2, range)

## ----label=plot-Fa-colMeans-----------------------------------------
colMeans(faClassicPcaReg@scores)
colMeans(faCovPcaRegMcd@scores)
colMeans(faClassicPcaReg@scores[15:75,])
colMeans(faCovPcaRegMcd@scores[15:75,])

## ----label=plot_Cov-compute, echo=FALSE, results='hide'-------------
rownames(hbk.x) = 1:75; hbk.x
covMcd = rrcov::CovRobust(x = hbk.x, control = "mcd"); covMcd

## ----label=hbk_myplotDD, echo=FALSE, results='hide', include=FALSE----
result = myplotDD(x = covMcd)

## ----label=plot_Cov, echo=FALSE, results='hide'---------------------
  ##
  ## All the following plots are OK for x = covMcd.
  ## The plot-methods is defined in the package rrcov.
  ##
  ## The figures generated in this code chunk is not
  ## reported in the paper.
  ##
  
  ##
  ## A distance-distance plot.
  ## We see that the robust (mahalanobis) distances are
## far larger than the (classical) mahalanobis distances.
## The outliers have large robust distances.
##
plot(x = covMcd, which = "dd")

##
## An index plot of the robust and mahalanobis distances.
## We also see that the robust distances are far larger than
## the mahalanobis distances and the outliers have large robust distances.
##
plot(x = covMcd, which = "distance", classic = T)

##
## A Chisquare QQ-plot of the robust and mahalanobis distances.
## We also see that the robust distances are far larger than
## the mahalanobis distances and the outliers have large robust distances.
##
plot(x = covMcd, which = "qqchi2", classic = T)

##
## Robust and classical 97.5% tolerance ellipses plot.
## The robust tolerance ellipse is tighter than the classical one.
## The robust tolerance ellipse separates the regular points and outliers.
##
plot(x = covMcd, which = "tolEllipsePlot", classic = T)

##
## Eigenvalues comparison plot.
## The eigenvalues of the robust method are much smaller than
## those of the classical method, and the largest 2 eigenvalues of
## the classical method decrease very fast.
##
plot(x = covMcd, which = "screeplot", classic = T)


## ----label=cutoff-id.n-sort.y-ind, echo=FALSE-----------------------
  cat("cutoff =", result$cutoff, "\n")
cat("id.n <- length(which(rd>cutoff))\n")
cat("id.n =", result$id.n, "\n")
cat("Here y is the robust distance (rd).\n")

Lst = list(x = result$sort.y$x[c(1:5, 71:75)], ix = result$sort.y$ix[c(1:5, 71:75)])
cat("sort.y = (To save space, only the smallest five and largest five
elements of sort.y$x and sort.y$ix are shown.)\n"); show(Lst)
cat("ind =\n"); show(result$ind)

## ----label=get_Fa---------------------------------------------------
robustfa::getEigenvalues(faCovPcaRegMcd)

## ----label=get_Fa2, echo=FALSE, results='hide'----------------------
robustfa::getCenter(faCovPcaRegMcd)
robustfa::getFa(faCovPcaRegMcd)
robustfa::getLoadings(faCovPcaRegMcd)
robustfa::getQuan(faCovPcaRegMcd)
robustfa::getScores(faCovPcaRegMcd)
robustfa::getSdev(faCovPcaRegMcd)

## ----label=hbk-cov-cor, echo=FALSE, results='hide'------------------
  ##
  ## robust factor analysis
  ## covariance vs correlation
  ## x vs scale(x)
  ##
  ## control = "auto", "mcd", "ogk", "m", "mve", "sde",
  ## "sfast", "surreal", "bisquare", "rocke" (these four are S-estimators)
  ##
  ## test the following:
  ## S_r != S_r_tilda? Yes!
  ## R_r == R_r_tilda?
## From the results of x = hbk.x, we guess that R_r == R_r_tilda!
##

##
## x = hbk.x
##
compute_cov_cor(x = hbk.x, control = "mcd") # Yes!
compute_cov_cor(x = hbk.x, control = "ogk") # Yes!
compute_cov_cor(x = hbk.x, control = "m") # Yes!
compute_cov_cor(x = hbk.x, control = "mve") # Yes!
compute_cov_cor(x = hbk.x, control = "sde") # small difference
compute_cov_cor(x = hbk.x, control = "sfast") # Yes!
compute_cov_cor(x = hbk.x, control = "surreal") # small difference
compute_cov_cor(x = hbk.x, control = "bisquare") # Yes!
compute_cov_cor(x = hbk.x, control = "rocke") # Yes!


## ----label=hbk-eigenvalues, echo=FALSE, results='hide'--------------
  ##
  ## The running matrices of (2), (3), and (4) are the same!
  ## The running matrices of (6) and (8) are the same!
  ## Consequently, the eigenvalues, loadings, importance of components are the same!
  ##
  
  ##
  ## x = hbk.x
  ##
  
  ## classical
covC = rrcov::CovClassic(x = hbk.x); covC
covC@cov # (1)
eigen(covC@cov)$values
cov2cor(covC@cov) # (2)
eigen(cov2cor(covC@cov))$values

## robust
covMcd = rrcov::CovRobust(x = hbk.x, control = "mcd"); covMcd
covMcd@cov # (5)
eigen(covMcd@cov)$values
cov2cor(covMcd@cov) # (6)
eigen(cov2cor(covMcd@cov))$values

##
## x = scale(hbk.x)
##

## classical
covC = rrcov::CovClassic(x = scale(hbk.x)); covC
covC@cov # (3)
eigen(covC@cov)$values
cov2cor(covC@cov) # (4)
eigen(cov2cor(covC@cov))$values

## robust
covMcd = rrcov::CovRobust(x = scale(hbk.x), control = "mcd"); covMcd
covMcd@cov # (7)
eigen(covMcd@cov)$values
cov2cor(covMcd@cov) # (8)
eigen(cov2cor(covMcd@cov))$values


## ----label=hbk-FaClassic-FaCov-factorScore, echo=FALSE, results='hide'----
##
## classical vs robust
## x = hbk.x or scale(hbk.x)
## cor = FALSE or TRUE
##
      
## (1) classical, x = hbk.x, cor = FALSE (covariance matrix)
faClassic1 = FaClassic(x = hbk.x, factors = 2, method = "pca",
                             scoresMethod = "regression"); faClassic1
summary(faClassic1)
plot(faClassic1, which = "factorScore", choices = 1:2)
    
## (2) classical, x = hbk.x, cor = TRUE (correlation matrix)
faClassic2 = FaClassic(x = hbk.x, factors = 2, cor = TRUE, method = "pca",
                           scoresMethod = "regression"); faClassic2
summary(faClassic2)
plot(faClassic2, which = "factorScore", choices = 1:2)
    
## (3) classical, x = scale(hbk.x), cor = FALSE (covariance matrix)
faClassic3 = FaClassic(x = scale(hbk.x), factors = 2, method = "pca",
                           scoresMethod = "regression"); faClassic3
summary(faClassic3)
plot(faClassic3, which = "factorScore", choices = 1:2)
    
## (4) classical, x = scale(hbk.x), cor = TRUE (correlation matrix)
faClassic4 = FaClassic(x = scale(hbk.x), factors = 2, cor = TRUE, method = "pca",
                           scoresMethod = "regression"); faClassic4
summary(faClassic4)
plot(faClassic4, which = "factorScore", choices = 1:2)
    
## (5) robust, x = hbk.x, cor = FALSE (covariance matrix)
faCov5 = FaCov(x = hbk.x, factors = 2, method = "pca",
                   scoresMethod = "regression", cov.control = rrcov::CovControlMcd()); faCov5
summary(faCov5)
plot(faCov5, which = "factorScore", choices = 1:2)
    
## (6) robust, x = hbk.x, cor = TRUE (correlation matrix)
faCov6 = FaCov(x = hbk.x, factors = 2, cor = TRUE, method = "pca",
                   scoresMethod = "regression", cov.control = rrcov::CovControlMcd()); faCov6
summary(faCov6)
plot(faCov6, which = "factorScore", choices = 1:2)
    
## (7) robust, x = scale(hbk.x), cor = FALSE (covariance matrix)
faCov7 = FaCov(x = scale(hbk.x), factors = 2, method = "pca",
                   scoresMethod = "regression", cov.control = rrcov::CovControlMcd()); faCov7
summary(faCov7)
plot(faCov7, which = "factorScore", choices = 1:2)
    
## (8) robust, x = scale(hbk.x), cor = TRUE (correlation matrix)
faCov8 = FaCov(x = scale(hbk.x), factors = 2, cor = TRUE, method = "pca",
                   scoresMethod = "regression", cov.control = rrcov::CovControlMcd()); faCov8
summary(faCov8)
plot(faCov8, which = "factorScore", choices = 1:2)
    

## ----label=hbk_vs_1_5, echo=FALSE, results='hide', include=FALSE----
##
## Classical and robust scatterplot of the first two factor scores of the hbk data.
## The 97.5% tolerance ellipses are superimposed.
##
      
## (1) vs (5)
usr <- par(mfrow = c(1,2))
cfaClassic <- list(center = c(0,0), cov = diag(faClassic1@eigenvalues[1:2]), n.obs = faClassic1@n.obs)
rrcov:::.myellipse(faClassic1@scores, xcov = cfaClassic, main = "Classical",
                       xlab = "Factor1", ylab = "Factor2", xlim = c(-40,40), ylim = c(-5,28), id.n = 0)
abline(v = 0)
abline(h = 0)
text(5,0,labels = "1-13", cex = 0.8)
text(0.5,6,labels = "14", cex = 0.8)
cfaCov <- list(center = c(0,0), cov = diag(faCov5@eigenvalues[1:2]), n.obs = faCov5@n.obs)
rrcov:::.myellipse(faCov5@scores, xcov = cfaCov, main = "Robust (MCD)",
                       xlab = "Factor1", ylab = "Factor2", xlim = c(-40,40), ylim = c(-5,28), id.n = 4)
text(22,9.5,labels = "1-10", cex = 0.8)
abline(v = 0)
abline(h = 0)
par(usr)
    
colMeans(faClassic1@scores)
colMeans(faCov5@scores)
colMeans(faClassic1@scores[15:75,])
colMeans(faCov5@scores[15:75,])

## ----label=hbk_vs_2_6, echo=FALSE, results='hide', include=FALSE----
## (2) vs (6)
usr <- par(mfrow = c(1,2))
cfaClassic <- list(center = c(0,0), cov = diag(faClassic2@eigenvalues[1:2]), n.obs = faClassic2@n.obs)
rrcov:::.myellipse(faClassic2@scores, xcov = cfaClassic, main = "Classical",
                       xlab = "Factor1", ylab = "Factor2", xlim = c(-40,40), ylim = c(-5,28), id.n = 0)
abline(v = 0)
abline(h = 0)
text(4,2,labels = "1-13", cex = 0.8)
text(5,-1,labels = "14", cex = 0.8)
cfaCov <- list(center = c(0,0), cov = diag(faCov6@eigenvalues[1:2]), n.obs = faCov6@n.obs)
rrcov:::.myellipse(faCov6@scores, xcov = cfaCov, main = "Robust (MCD)",
                       xlab = "Factor1", ylab = "Factor2", xlim = c(-40,40), ylim = c(-5,28), id.n = 4)
text(22,8.5,labels = "1-10", cex = 0.8)
abline(v = 0)
abline(h = 0)
par(usr)
    
colMeans(faClassic2@scores)
colMeans(faCov6@scores)
colMeans(faClassic2@scores[15:75,])
colMeans(faCov6@scores[15:75,])
    

## ----label=hbk_vs_3_7, echo=FALSE, results='hide', include=FALSE----
## (3) vs (7)
usr <- par(mfrow = c(1,2))
cfaClassic <- list(center = c(0,0), cov = diag(faClassic3@eigenvalues[1:2]), n.obs = faClassic3@n.obs)
rrcov:::.myellipse(faClassic3@scores, xcov = cfaClassic, main = "Classical",
                       xlab = "Factor1", ylab = "Factor2", xlim = c(-40,40), ylim = c(-5,28), id.n = 0)
abline(v = 0)
abline(h = 0)
text(4,2,labels = "1-13", cex = 0.8)
text(5,-1,labels = "14", cex = 0.8)
cfaCov <- list(center = c(0,0), cov = diag(faCov7@eigenvalues[1:2]), n.obs = faCov7@n.obs)
rrcov:::.myellipse(faCov7@scores, xcov = cfaCov, main = "Robust (MCD)",
                       xlab = "Factor1", ylab = "Factor2", xlim = c(-40,40), ylim = c(-5,28), id.n = 4)
text(5,15,labels = "1-10", cex = 0.8)
abline(v = 0)
abline(h = 0)
par(usr)
    
colMeans(faClassic3@scores)
colMeans(faCov7@scores)
colMeans(faClassic3@scores[15:75,])
colMeans(faCov7@scores[15:75,])
    

## ----label=hbk_vs_4_8, echo=FALSE, results='hide', include=FALSE----
## (4) vs (8)
usr <- par(mfrow = c(1,2))
cfaClassic <- list(center = c(0,0), cov = diag(faClassic4@eigenvalues[1:2]), n.obs = faClassic4@n.obs)
rrcov:::.myellipse(faClassic4@scores, xcov = cfaClassic, main = "Classical",
                   xlab = "Factor1", ylab = "Factor2", xlim = c(-40,40), ylim = c(-5,28), id.n = 0)
abline(v = 0)
abline(h = 0)
text(4,2,labels = "1-13", cex = 0.8)
text(5,-1,labels = "14", cex = 0.8)
cfaCov <- list(center = c(0,0), cov = diag(faCov8@eigenvalues[1:2]), n.obs = faCov8@n.obs)
rrcov:::.myellipse(faCov8@scores, xcov = cfaCov, main = "Robust (MCD)",
                       xlab = "Factor1", ylab = "Factor2", xlim = c(-40,40), ylim = c(-5,28), id.n = 4)
text(22,8,labels = "1-10", cex = 0.8)
abline(v = 0)
abline(h = 0)
par(usr)
    
colMeans(faClassic4@scores)
colMeans(faCov8@scores)
colMeans(faClassic4@scores[15:75,])
colMeans(faCov8@scores[15:75,])
    
## xlim = c(-40,40), ylim = c(-5,28) # the first ellipse is large
## xlim = c(-2,33), ylim = c(-2,28)
apply(rbind(faClassic1@scores, faCov5@scores), 2, range)
apply(rbind(faClassic2@scores, faCov6@scores), 2, range)
apply(rbind(faClassic3@scores, faCov7@scores), 2, range)
apply(rbind(faClassic4@scores, faCov8@scores), 2, range)
    

## ----label=set-prompt, echo=FALSE, results='hide'-------------------
##
## set the prompt to "R> " and the continuation to "+ "
##
options(prompt = "R> ", continue = "+  ", width = 70, useFancyQuotes = FALSE)

## ----label=library-hbk-stock6112, echo=FALSE, results='hide'--------
##
## Load the 'robustfa' package and two data sets
##
library("robustfa")
data("hbk")
hbk.x = hbk[,1:3] # take only the X part
rownames(hbk.x) = 1:75

data("stock611")
stock611$name = 1:611; stock611
stock608 = stock611[-c(92,2,337),]
stock604 = stock611[-c(92,2,337,338,379,539,79),]
R611 = cor(stock611[,3:12]); R611

## ----label=FaCov.default2, results='hide'---------------------------
##
## faCovPcaRegMcd is obtained from FaCov.default
##
faCovPcaRegMcd = FaCov(x = hbk.x, factors = 2, method = "pca",
                   scoresMethod = "regression", cov.control = rrcov::CovControlMcd());
faCovPcaRegMcd

## ----label=show-print-summary_FaB, echo=FALSE, results='hide'-------
faCovPcaRegMcd
summary(faCovPcaRegMcd)

## ----label=FaCov.formula2, results='hide'---------------------------
faCovForPcaRegMcd = FaCov(~., data = as.data.frame(hbk.x),
                      factors = 2, method = "pca", scoresMethod = "regression",
                      cov.control = rrcov::CovControlMcd())

## ----label=show-print-summary_FaB2, echo=FALSE, results='hide'------
faCovForPcaRegMcd
summary(faCovForPcaRegMcd)

## ----label=show-print-summary_FaB3----------------------------------
class(faCovPcaRegMcd)

## ----label=show-print-summary_FaB4----------------------------------
print(faCovPcaRegMcd)

## ----label=show-print-summary_FaB5, echo=FALSE, results='hide'------
faCovPcaRegMcd
myFaPrint(faCovPcaRegMcd)

## ----label=show-print-summary_FaB6----------------------------------
summaryFaCovPcaRegMcd = summary(faCovPcaRegMcd);
summaryFaCovPcaRegMcd

## ----label=show-print-summary_FaB7, echo=FALSE, results='hide'------
print(summaryFaCovPcaRegMcd)
class(summaryFaCovPcaRegMcd)

## ----label=predict_FaB, results='hide'------------------------------
predict(faCovPcaRegMcd)

## ----label=predict_FaB2, echo=FALSE---------------------------------
predict(faCovPcaRegMcd)[c(1:5, 71:75),]

## ----label=predict_FaB3, results='hide'-----------------------------
newdata = hbk.x[1, ]
cor = FALSE # the default
newdata = { if (cor == TRUE)
# standardized transformation
scale(newdata, center = faCovPcaRegMcd@center,
  scale = sqrt(diag(faCovPcaRegMcd@covariance)))
else # cor == FALSE
# centralized transformation
scale(newdata, center = faCovPcaRegMcd@center, scale = FALSE)
}

## ----label=predict_FaB4---------------------------------------------
prediction = predict(faCovPcaRegMcd, newdata = newdata)
prediction

## ----label=plot-Fa-FaClassic-FaCovB---------------------------------
faClassicPcaReg = FaClassic(x = hbk.x, factors = 2, method = "pca",
                        scoresMethod = "regression"); faClassicPcaReg
summary(faClassicPcaReg)

## ----label=plot-Fa-FaClassic-FaCovB2, echo=FALSE, results='hide'----
##
## FaCov
##
faCovPcaRegMcd = FaCov(x = hbk.x, factors = 2, method = "pca",
                   scoresMethod = "regression", cov.control = rrcov::CovControlMcd()); faCovPcaRegMcd
summary(faCovPcaRegMcd)

## ----label=hbk_factorScoreB, echo=TRUE, results='hide', include=FALSE----
usr <- par(mfrow=c(1,2))
plot(faClassicPcaReg, which = "factorScore", choices = 1:2)
plot(faCovPcaRegMcd, which = "factorScore", choices = 1:2)
par(usr)

## ----label=hbk_screeplotB, echo=TRUE, results='hide', include=FALSE----
usr <- par(mfrow=c(1,2))
plot(faClassicPcaReg, which = "screeplot")
plot(faCovPcaRegMcd, which = "screeplot")
par(usr)

## ----label=hbk_vs_classical_robustB, echo=FALSE, results='hide', include=FALSE----
##
## Plot of the first two factors of hbk and
## 97.5% tolerance ellipse plot: classical and robust.
##
cfaClassicPcaReg <- list(center = c(0,0), cov = diag(faClassicPcaReg@eigenvalues[1:2]), n.obs = faClassicPcaReg@n.obs)
cfaCovPcaRegMcd <- list(center = c(0,0), cov = diag(faCovPcaRegMcd@eigenvalues[1:2]), n.obs = faCovPcaRegMcd@n.obs)

usr <- par(mfrow=c(1,2))
rrcov:::.myellipse(faClassicPcaReg@scores, xcov = cfaClassicPcaReg,
             main = "Classical, method = \"pca\"", xlab = "Factor1", ylab = "Factor2", id.n = 0,
             xlim = c(-40,40), ylim = c(-5,15))
abline(v = 0)
abline(h = 0)
text(5,0,labels = "1-13", cex = 0.8)
text(0.5,6,labels = "14", cex = 0.8)
rrcov:::.myellipse(faCovPcaRegMcd@scores, xcov = cfaCovPcaRegMcd,
             main = "Robust (MCD), method = \"pca\"", xlab = "Factor1", ylab = "Factor2", id.n = 4,
             xlim = c(-40,40), ylim = c(-5,15))
text(22,9.5,labels = "1-10", cex = 0.8)
abline(v = 0)
abline(h = 0)
par(usr)

## xlim = c(-40,40), ylim = c(-5,15) # the first ellipse is large
## xlim = c(-2,32), ylim = c(-2,14)
apply(rbind(faClassicPcaReg@scores, faCovPcaRegMcd@scores), 2, range)

## ----label=plot-Fa-colMeansB----------------------------------------
colMeans(faClassicPcaReg@scores)
colMeans(faCovPcaRegMcd@scores)
colMeans(faClassicPcaReg@scores[15:75,])
colMeans(faCovPcaRegMcd@scores[15:75,])

## ----label=plot_Cov-computeB, echo=FALSE, results='hide'------------
rownames(hbk.x) = 1:75; hbk.x
covMcd = rrcov::CovRobust(x = hbk.x, control = "mcd"); covMcd

## ----label=hbk_myplotDDB, echo=FALSE, results='hide', include=FALSE----
result = myplotDD(x = covMcd)

## ----label=plot_CovB, echo=FALSE, results='hide'--------------------
##
## All the following plots are OK for x = covMcd.
## The plot-methods is defined in the package rrcov.
##
## The figures generated in this code chunk is not
## reported in the paper.
##

##
## A distance-distance plot.
## We see that the robust (mahalanobis) distances are
## far larger than the (classical) mahalanobis distances.
## The outliers have large robust distances.
##
plot(x = covMcd, which = "dd")

##
## An index plot of the robust and mahalanobis distances.
## We also see that the robust distances are far larger than
## the mahalanobis distances and the outliers have large robust distances.
##
plot(x = covMcd, which = "distance", classic = T)

##
## A Chisquare QQ-plot of the robust and mahalanobis distances.
## We also see that the robust distances are far larger than
## the mahalanobis distances and the outliers have large robust distances.
##
plot(x = covMcd, which = "qqchi2", classic = T)

##
## Robust and classical 97.5% tolerance ellipses plot.
## The robust tolerance ellipse is tighter than the classical one.
## The robust tolerance ellipse separates the regular points and outliers.
##
plot(x = covMcd, which = "tolEllipsePlot", classic = T)

##
## Eigenvalues comparison plot.
## The eigenvalues of the robust method are much smaller than
## those of the classical method, and the largest 2 eigenvalues of
## the classical method decrease very fast.
##
plot(x = covMcd, which = "screeplot", classic = T)


## ----label=cutoff-id.n-sort.y-indB, echo=FALSE----------------------
cat("cutoff =", result$cutoff, "\n")
cat("id.n <- length(which(rd>cutoff))\n")
cat("id.n =", result$id.n, "\n")
cat("Here y is the robust distance (rd).\n")

Lst = list(x = result$sort.y$x[c(1:5, 71:75)], ix = result$sort.y$ix[c(1:5, 71:75)])
cat("sort.y = (To save space, only the smallest five and largest five
elements of sort.y$x and sort.y$ix are shown.)\n"); show(Lst)
cat("ind =\n"); show(result$ind)

## ----label=get_FaC--------------------------------------------------
robustfa::getEigenvalues(faCovPcaRegMcd)

## ----label=get_FaC2, echo=FALSE, results='hide'---------------------
robustfa::getCenter(faCovPcaRegMcd)
robustfa::getFa(faCovPcaRegMcd)
robustfa::getLoadings(faCovPcaRegMcd)
robustfa::getQuan(faCovPcaRegMcd)
robustfa::getScores(faCovPcaRegMcd)
robustfa::getSdev(faCovPcaRegMcd)

## ----label=hbk-cov-corB, echo=FALSE, results='hide'-----------------
##
## robust factor analysis
## covariance vs correlation
## x vs scale(x)
##
## control = "auto", "mcd", "ogk", "m", "mve", "sde",
## "sfast", "surreal", "bisquare", "rocke" (these four are S-estimators)
##
## test the following:
## S_r != S_r_tilda? Yes!
## R_r == R_r_tilda?
## From the results of x = hbk.x, we guess that R_r == R_r_tilda!
##

##
## x = hbk.x
##
compute_cov_cor(x = hbk.x, control = "mcd") # Yes!
compute_cov_cor(x = hbk.x, control = "ogk") # Yes!
compute_cov_cor(x = hbk.x, control = "m") # Yes!
compute_cov_cor(x = hbk.x, control = "mve") # Yes!
compute_cov_cor(x = hbk.x, control = "sde") # small difference
compute_cov_cor(x = hbk.x, control = "sfast") # Yes!
compute_cov_cor(x = hbk.x, control = "surreal") # small difference
compute_cov_cor(x = hbk.x, control = "bisquare") # Yes!
compute_cov_cor(x = hbk.x, control = "rocke") # Yes!


## ----label=hbk-eigenvaluesB, echo=FALSE, results='hide'-------------
##
## The running matrices of (2), (3), and (4) are the same!
## The running matrices of (6) and (8) are the same!
## Consequently, the eigenvalues, loadings, importance of components are the same!
##

##
## x = hbk.x
##

## classical
covC = rrcov::CovClassic(x = hbk.x); covC
covC@cov # (1)
eigen(covC@cov)$values
cov2cor(covC@cov) # (2)
eigen(cov2cor(covC@cov))$values

## robust
covMcd = rrcov::CovRobust(x = hbk.x, control = "mcd"); covMcd
covMcd@cov # (5)
eigen(covMcd@cov)$values
cov2cor(covMcd@cov) # (6)
eigen(cov2cor(covMcd@cov))$values

##
## x = scale(hbk.x)
##

## classical
covC = rrcov::CovClassic(x = scale(hbk.x)); covC
covC@cov # (3)
eigen(covC@cov)$values
cov2cor(covC@cov) # (4)
eigen(cov2cor(covC@cov))$values

## robust
covMcd = rrcov::CovRobust(x = scale(hbk.x), control = "mcd"); covMcd
covMcd@cov # (7)
eigen(covMcd@cov)$values
cov2cor(covMcd@cov) # (8)
eigen(cov2cor(covMcd@cov))$values


## ----label=hbk-FaClassic-FaCov-factorScoreB, echo=FALSE, results='hide'----
##
## classical vs robust
## x = hbk.x or scale(hbk.x)
## cor = FALSE or TRUE
##

## (1) classical, x = hbk.x, cor = FALSE (covariance matrix)
faClassic1 = FaClassic(x = hbk.x, factors = 2, method = "pca",
                       scoresMethod = "regression"); faClassic1
summary(faClassic1)
plot(faClassic1, which = "factorScore", choices = 1:2)

## (2) classical, x = hbk.x, cor = TRUE (correlation matrix)
faClassic2 = FaClassic(x = hbk.x, factors = 2, cor = TRUE, method = "pca",
                     scoresMethod = "regression"); faClassic2
summary(faClassic2)
plot(faClassic2, which = "factorScore", choices = 1:2)

## (3) classical, x = scale(hbk.x), cor = FALSE (covariance matrix)
faClassic3 = FaClassic(x = scale(hbk.x), factors = 2, method = "pca",
                     scoresMethod = "regression"); faClassic3
summary(faClassic3)
plot(faClassic3, which = "factorScore", choices = 1:2)

## (4) classical, x = scale(hbk.x), cor = TRUE (correlation matrix)
faClassic4 = FaClassic(x = scale(hbk.x), factors = 2, cor = TRUE, method = "pca",
                     scoresMethod = "regression"); faClassic4
summary(faClassic4)
plot(faClassic4, which = "factorScore", choices = 1:2)

## (5) robust, x = hbk.x, cor = FALSE (covariance matrix)
faCov5 = FaCov(x = hbk.x, factors = 2, method = "pca",
             scoresMethod = "regression", cov.control = rrcov::CovControlMcd()); faCov5
summary(faCov5)
plot(faCov5, which = "factorScore", choices = 1:2)

## (6) robust, x = hbk.x, cor = TRUE (correlation matrix)
faCov6 = FaCov(x = hbk.x, factors = 2, cor = TRUE, method = "pca",
             scoresMethod = "regression", cov.control = rrcov::CovControlMcd()); faCov6
summary(faCov6)
plot(faCov6, which = "factorScore", choices = 1:2)

## (7) robust, x = scale(hbk.x), cor = FALSE (covariance matrix)
faCov7 = FaCov(x = scale(hbk.x), factors = 2, method = "pca",
             scoresMethod = "regression", cov.control = rrcov::CovControlMcd()); faCov7
summary(faCov7)
plot(faCov7, which = "factorScore", choices = 1:2)

## (8) robust, x = scale(hbk.x), cor = TRUE (correlation matrix)
faCov8 = FaCov(x = scale(hbk.x), factors = 2, cor = TRUE, method = "pca",
             scoresMethod = "regression", cov.control = rrcov::CovControlMcd()); faCov8
summary(faCov8)
plot(faCov8, which = "factorScore", choices = 1:2)


## ----label=hbk_vs_1_5B, echo=FALSE, results='hide', include=FALSE----
##
## Classical and robust scatterplot of the first two factor scores of the hbk data.
## The 97.5% tolerance ellipses are superimposed.
##

## (1) vs (5)
usr <- par(mfrow = c(1,2))
cfaClassic <- list(center = c(0,0), cov = diag(faClassic1@eigenvalues[1:2]), n.obs = faClassic1@n.obs)
rrcov:::.myellipse(faClassic1@scores, xcov = cfaClassic, main = "Classical",
                 xlab = "Factor1", ylab = "Factor2", xlim = c(-40,40), ylim = c(-5,28), id.n = 0)
abline(v = 0)
abline(h = 0)
text(5,0,labels = "1-13", cex = 0.8)
text(0.5,6,labels = "14", cex = 0.8)
cfaCov <- list(center = c(0,0), cov = diag(faCov5@eigenvalues[1:2]), n.obs = faCov5@n.obs)
rrcov:::.myellipse(faCov5@scores, xcov = cfaCov, main = "Robust (MCD)",
                 xlab = "Factor1", ylab = "Factor2", xlim = c(-40,40), ylim = c(-5,28), id.n = 4)
text(22,9.5,labels = "1-10", cex = 0.8)
abline(v = 0)
abline(h = 0)
par(usr)

colMeans(faClassic1@scores)
colMeans(faCov5@scores)
colMeans(faClassic1@scores[15:75,])
colMeans(faCov5@scores[15:75,])

## ----label=hbk_vs_2_6B, echo=FALSE, results='hide', include=FALSE----
## (2) vs (6)
usr <- par(mfrow = c(1,2))
cfaClassic <- list(center = c(0,0), cov = diag(faClassic2@eigenvalues[1:2]), n.obs = faClassic2@n.obs)
rrcov:::.myellipse(faClassic2@scores, xcov = cfaClassic, main = "Classical",
                 xlab = "Factor1", ylab = "Factor2", xlim = c(-40,40), ylim = c(-5,28), id.n = 0)
abline(v = 0)
abline(h = 0)
text(4,2,labels = "1-13", cex = 0.8)
text(5,-1,labels = "14", cex = 0.8)
cfaCov <- list(center = c(0,0), cov = diag(faCov6@eigenvalues[1:2]), n.obs = faCov6@n.obs)
rrcov:::.myellipse(faCov6@scores, xcov = cfaCov, main = "Robust (MCD)",
                 xlab = "Factor1", ylab = "Factor2", xlim = c(-40,40), ylim = c(-5,28), id.n = 4)
text(22,8.5,labels = "1-10", cex = 0.8)
abline(v = 0)
abline(h = 0)
par(usr)

colMeans(faClassic2@scores)
colMeans(faCov6@scores)
colMeans(faClassic2@scores[15:75,])
colMeans(faCov6@scores[15:75,])


## ----label=hbk_vs_3_7B, echo=FALSE, results='hide', include=FALSE----
## (3) vs (7)
usr <- par(mfrow = c(1,2))
cfaClassic <- list(center = c(0,0), cov = diag(faClassic3@eigenvalues[1:2]), n.obs = faClassic3@n.obs)
rrcov:::.myellipse(faClassic3@scores, xcov = cfaClassic, main = "Classical",
                 xlab = "Factor1", ylab = "Factor2", xlim = c(-40,40), ylim = c(-5,28), id.n = 0)
abline(v = 0)
abline(h = 0)
text(4,2,labels = "1-13", cex = 0.8)
text(5,-1,labels = "14", cex = 0.8)
cfaCov <- list(center = c(0,0), cov = diag(faCov7@eigenvalues[1:2]), n.obs = faCov7@n.obs)
rrcov:::.myellipse(faCov7@scores, xcov = cfaCov, main = "Robust (MCD)",
                 xlab = "Factor1", ylab = "Factor2", xlim = c(-40,40), ylim = c(-5,28), id.n = 4)
text(5,15,labels = "1-10", cex = 0.8)
abline(v = 0)
abline(h = 0)
par(usr)

colMeans(faClassic3@scores)
colMeans(faCov7@scores)
colMeans(faClassic3@scores[15:75,])
colMeans(faCov7@scores[15:75,])


## ----label=hbk_vs_4_8B, echo=FALSE, results='hide', include=FALSE----
## (4) vs (8)
usr <- par(mfrow = c(1,2))
cfaClassic <- list(center = c(0,0), cov = diag(faClassic4@eigenvalues[1:2]), n.obs = faClassic4@n.obs)
rrcov:::.myellipse(faClassic4@scores, xcov = cfaClassic, main = "Classical",
                 xlab = "Factor1", ylab = "Factor2", xlim = c(-40,40), ylim = c(-5,28), id.n = 0)
abline(v = 0)
abline(h = 0)
text(4,2,labels = "1-13", cex = 0.8)
text(5,-1,labels = "14", cex = 0.8)
cfaCov <- list(center = c(0,0), cov = diag(faCov8@eigenvalues[1:2]), n.obs = faCov8@n.obs)
rrcov:::.myellipse(faCov8@scores, xcov = cfaCov, main = "Robust (MCD)",
                 xlab = "Factor1", ylab = "Factor2", xlim = c(-40,40), ylim = c(-5,28), id.n = 4)
text(22,8,labels = "1-10", cex = 0.8)
abline(v = 0)
abline(h = 0)
par(usr)

colMeans(faClassic4@scores)
colMeans(faCov8@scores)
colMeans(faClassic4@scores[15:75,])
colMeans(faCov8@scores[15:75,])

## xlim = c(-40,40), ylim = c(-5,28) # the first ellipse is large
## xlim = c(-2,33), ylim = c(-2,28)
apply(rbind(faClassic1@scores, faCov5@scores), 2, range)
apply(rbind(faClassic2@scores, faCov6@scores), 2, range)
apply(rbind(faClassic3@scores, faCov7@scores), 2, range)
apply(rbind(faClassic4@scores, faCov8@scores), 2, range)


## ----label=stock611-cov-corB, echo=FALSE, results='hide'------------
##
## robust factor analysis
## covariance vs correlation
## x vs scale(x)
##
## control = "auto", "mcd", "ogk", "m", "mve", "sde",
## "sfast", "surreal", "bisquare", "rocke" (these four are S-estimators)
##
## test the following:
## S_r != S_r_tilda? Yes!
## R_r == R_r_tilda?
##
## x = stock611[,3:12]
##
## We can not compute cov_x, S_r, and R_r.
## However, we can compute cov_scale_x, S_r_tilda, and R_r_tilda.
##
## The error message for control = "mcd", "m", "mve", "sde", "sfast" are:
## Error in solve.default(cov, ...) :
##   system is computationally singular: reciprocal condition number = 1.34036e-21
##

## compute_cov_cor(x = stock611[,3:12], control = "mcd") # Error
compute_cov_cor(x = stock611[,3:12], control = "ogk") # Yes!
## compute_cov_cor(x = stock611[,3:12], control = "m") # Error
## compute_cov_cor(x = stock611[,3:12], control = "mve") # Error
## compute_cov_cor(x = stock611[,3:12], control = "sde") # Error
## compute_cov_cor(x = stock611[,3:12], control = "sfast") # Error
## compute_cov_cor(x = stock611[,3:12], control = "surreal") # computation extensive
compute_cov_cor(x = stock611[,3:12], control = "bisquare") # Yes!
compute_cov_cor(x = stock611[,3:12], control = "rocke") # Yes!


##
## For x = stock611[,3:12], control = "mcd", "m", "mve", "sde", "sfast" get error messages.
## Thus we CAN NOT get results for combinations (5) and (6) for these robust estimators.
##
## Error in solve.default(cov, ...) :
##  system is computationally singular: reciprocal condition number = 1.37016e-21
##
## cov_x_mcd   = rrcov::CovRobust(x = stock611[,3:12], control = "mcd");   cov_x_mcd
## cov_x_m     = rrcov::CovRobust(x = stock611[,3:12], control = "m");     cov_x_m
## cov_x_mve   = rrcov::CovRobust(x = stock611[,3:12], control = "mve");   cov_x_mve
## cov_x_sde   = rrcov::CovRobust(x = stock611[,3:12], control = "sde");   cov_x_sde
## cov_x_sfast = rrcov::CovRobust(x = stock611[,3:12], control = "sfast"); cov_x_sfast

##
## For x = scale(stock611[,3:12]), control = "mcd", "m", "mve", "sde", "sfast" are OK.
## Thus we CAN get results for combinations (7) and (8) for these robust estimators.
##
cov_scale_x_mcd   = rrcov::CovRobust(x = scale(stock611[,3:12]), control = "mcd");   cov_scale_x_mcd
cov_scale_x_m     = rrcov::CovRobust(x = scale(stock611[,3:12]), control = "m");     cov_scale_x_m
cov_scale_x_mve   = rrcov::CovRobust(x = scale(stock611[,3:12]), control = "mve");   cov_scale_x_mve
cov_scale_x_sde   = rrcov::CovRobust(x = scale(stock611[,3:12]), control = "sde");   cov_scale_x_sde
cov_scale_x_sfast = rrcov::CovRobust(x = scale(stock611[,3:12]), control = "sfast"); cov_scale_x_sfast

## ----label=stock611-eigenvaluesB, echo=FALSE, results='hide'--------
##
## The running matrices of (2), (3), and (4) are the same!
## The running matrices of (6) and (8) are the same!
## Consequently, the eigenvalues, loadings, importance of components are the same!
##

##
## x = stock611[,3:12])
##

## classical
covC = rrcov::CovClassic(x = stock611[,3:12]); covC # covC = R611
eigen(covC@cov)$values
eigen(cov2cor(covC@cov))$values

## robust
covOgk = rrcov::CovRobust(x = stock611[,3:12], control = "ogk"); covOgk
eigen(covOgk@cov)$values
eigen(cov2cor(covOgk@cov))$values

##
## x = scale(stock611[,3:12])
##

## classical
covC = rrcov::CovClassic(x = scale(stock611[,3:12])); covC # covC = R611
eigen(covC@cov)$values
eigen(cov2cor(covC@cov))$values

## robust
covOgk = rrcov::CovRobust(x = scale(stock611[,3:12]), control = "ogk"); covOgk
eigen(covOgk@cov)$values
eigen(cov2cor(covOgk@cov))$values


## ----label=stock611-FaClassic-FaCov-factorScoreB, echo=FALSE, results='hide'----
##
## classical vs robust
## x = stock611[,3:12] or scale(stock611[,3:12])
## cor = FALSE or TRUE
##

## (1) classical, x = stock611[,3:12], cor = FALSE (covariance matrix)
##
## Error in solve.default(S) :
##   system is computationally singular: reciprocal condition number = 1.31917e-23
##
## faClassic1 = FaClassic(x = stock611[,3:12], factors = 2, method = "pca",
## scoresMethod = "regression"); faClassic1
## summary(faClassic1)
## plot(faClassic1, which = "factorScore", choices = 1:2)

## (2) classical, x = stock611[,3:12], cor = TRUE (correlation matrix)
faClassic2 = FaClassic(x = stock611[,3:12], factors = 2, cor = TRUE, method = "pca",
                     scoresMethod = "regression"); faClassic2
summary(faClassic2)
plot(faClassic2, which = "factorScore", choices = 1:2)

## (3) classical, x = scale(stock611[,3:12]), cor = FALSE (covariance matrix)
faClassic3 = FaClassic(x = scale(stock611[,3:12]), factors = 2, method = "pca",
                     scoresMethod = "regression"); faClassic3
summary(faClassic3)
plot(faClassic3, which = "factorScore", choices = 1:2)

## (4) classical, x = scale(stock611[,3:12]), cor = TRUE (correlation matrix)
faClassic4 = FaClassic(x = scale(stock611[,3:12]), factors = 2, cor = TRUE, method = "pca",
                     scoresMethod = "regression"); faClassic4
summary(faClassic4)
plot(faClassic4, which = "factorScore", choices = 1:2)

## (5) robust, x = stock611[,3:12], cor = FALSE (covariance matrix)
##
## Error in solve.default(S) :
##   system is computationally singular: reciprocal condition number = 1.17808e-21
##
## faCov5 = FaCov(x = stock611[,3:12], factors = 2, method = "pca",
## scoresMethod = "regression", cov.control = CovControlOgk()); faCov5
## summary(faCov5)
## plot(faCov5, which = "factorScore", choices = 1:2)

## (6) robust, x = stock611[,3:12], cor = TRUE (correlation matrix)
faCov6 = FaCov(x = stock611[,3:12], factors = 2, cor = TRUE, method = "pca",
             scoresMethod = "regression", cov.control = rrcov::CovControlOgk()); faCov6
summary(faCov6)
plot(faCov6, which = "factorScore", choices = 1:2)

## (7) robust, x = scale(stock611[,3:12]), cor = FALSE (covariance matrix)
faCov7 = FaCov(x = scale(stock611[,3:12]), factors = 2, method = "pca",
             scoresMethod = "regression", cov.control = rrcov::CovControlOgk()); faCov7
summary(faCov7)
plot(faCov7, which = "factorScore", choices = 1:2)

## (8) robust, x = scale(stock611[,3:12]), cor = TRUE (correlation matrix)
faCov8 = FaCov(x = scale(stock611[,3:12]), factors = 2, cor = TRUE, method = "pca",
             scoresMethod = "regression", cov.control = rrcov::CovControlOgk()); faCov8
summary(faCov8)
plot(faCov8, which = "factorScore", choices = 1:2)


## ----label=stock611-factorScore-ellipses, echo=FALSE, results='hide'----
##
## Classical and robust scatterplot of the first two factor scores of the stock611 data.
## The 97.5% tolerance ellipses are superimposed.
##

## (1) vs (5)
## not available

## (2) vs (6)
usr <- par(mfrow = c(1,2))
cfaClassic <- list(center = c(0,0), cov = diag(faClassic2@eigenvalues[1:2]), n.obs = faClassic2@n.obs)
rrcov:::.myellipse(faClassic2@scores, xcov = cfaClassic, main = "Classical",
                 xlab = "Factor1", ylab = "Factor2", xlim = c(-20,40), ylim = c(-20,900), id.n = 0)
abline(v = 0)
abline(h = 0)
cfaCov <- list(center = c(0,0), cov = diag(faCov6@eigenvalues[1:2]), n.obs = faCov6@n.obs)
rrcov:::.myellipse(faCov6@scores, xcov = cfaCov, main = "Robust (OGK)",
                 xlab = "Factor1", ylab = "Factor2", xlim = c(-20,40), ylim = c(-20,900), id.n = 0)
abline(v = 0)
abline(h = 0)
par(usr)

## all observations
colMeans(faClassic2@scores)
colMeans(faCov6@scores)

## good observations
colMeans(faClassic2@scores[-result$ind, ])
colMeans(faCov6@scores[-result$ind, ])

## (3) vs (7)
usr <- par(mfrow = c(1,2))
cfaClassic <- list(center = c(0,0), cov = diag(faClassic3@eigenvalues[1:2]), n.obs = faClassic3@n.obs)
rrcov:::.myellipse(faClassic3@scores, xcov = cfaClassic, main = "Classical",
                 xlab = "Factor1", ylab = "Factor2", xlim = c(-20,40), ylim = c(-20,900), id.n = 0)
abline(v = 0)
abline(h = 0)
cfaCov <- list(center = c(0,0), cov = diag(faCov7@eigenvalues[1:2]), n.obs = faCov7@n.obs)
rrcov:::.myellipse(faCov7@scores, xcov = cfaCov, main = "Robust (OGK)",
                 xlab = "Factor1", ylab = "Factor2", xlim = c(-20,40), ylim = c(-20,900), id.n = 0)
abline(v = 0)
abline(h = 0)
par(usr)

## all observations
colMeans(faClassic3@scores)
colMeans(faCov7@scores)

## good observations
colMeans(faClassic3@scores[-result$ind, ])
colMeans(faCov7@scores[-result$ind, ])

## (4) vs (8)
usr <- par(mfrow = c(1,2))
cfaClassic <- list(center = c(0,0), cov = diag(faClassic4@eigenvalues[1:2]), n.obs = faClassic4@n.obs)
rrcov:::.myellipse(faClassic4@scores, xcov = cfaClassic, main = "Classical",
                 xlab = "Factor1", ylab = "Factor2", xlim = c(-20,40), ylim = c(-20,900), id.n = 0)
abline(v = 0)
abline(h = 0)
cfaCov <- list(center = c(0,0), cov = diag(faCov8@eigenvalues[1:2]), n.obs = faCov8@n.obs)
rrcov:::.myellipse(faCov8@scores, xcov = cfaCov, main = "Robust (OGK)",
                 xlab = "Factor1", ylab = "Factor2", xlim = c(-20,40), ylim = c(-20,900), id.n = 0)
abline(v = 0)
abline(h = 0)
par(usr)

## all observations
colMeans(faClassic4@scores)
colMeans(faCov8@scores)

## good observations
colMeans(faClassic4@scores[-result$ind, ])
colMeans(faCov8@scores[-result$ind, ])

## xlim = c(-20,40), ylim = c(-20,900)
apply(rbind(faClassic2@scores, faCov6@scores), 2, range)
apply(rbind(faClassic3@scores, faCov7@scores), 2, range)
apply(rbind(faClassic4@scores, faCov8@scores), 2, range)


## ----label=stock611_vs_ZoomIn_2_6, echo=FALSE, results='hide', include=FALSE----
##
## Classical and robust scatterplot of the first two factor scores of the stock611 data.
## The 97.5% tolerance ellipses are superimposed.
##
## ZoomIn
## xlim = c(-10,10), ylim = c(-10,10)
##

## (2) vs (6)
usr <- par(mfrow = c(1,2))
cfaClassic <- list(center = c(0,0), cov = diag(faClassic2@eigenvalues[1:2]), n.obs = faClassic2@n.obs)
rrcov:::.myellipse(faClassic2@scores, xcov = cfaClassic, main = "Classical",
                 xlab = "Factor1", ylab = "Factor2", xlim = c(-10,10), ylim = c(-10,10), id.n = 0)
abline(v = 0)
abline(h = 0)
cfaCov <- list(center = c(0,0), cov = diag(faCov6@eigenvalues[1:2]), n.obs = faCov6@n.obs)
rrcov:::.myellipse(faCov6@scores, xcov = cfaCov, main = "Robust (OGK)",
                 xlab = "Factor1", ylab = "Factor2", xlim = c(-10,10), ylim = c(-10,10), id.n = 0)
abline(v = 0)
abline(h = 0)
par(usr)


## ----label=stock611_vs_ZoomIn_3_7, echo=FALSE, results='hide', include=FALSE----
## (3) vs (7)
usr <- par(mfrow = c(1,2))
cfaClassic <- list(center = c(0,0), cov = diag(faClassic3@eigenvalues[1:2]), n.obs = faClassic3@n.obs)
rrcov:::.myellipse(faClassic3@scores, xcov = cfaClassic, main = "Classical",
                 xlab = "Factor1", ylab = "Factor2", xlim = c(-10,10), ylim = c(-10,10), id.n = 0)
abline(v = 0)
abline(h = 0)
cfaCov <- list(center = c(0,0), cov = diag(faCov7@eigenvalues[1:2]), n.obs = faCov7@n.obs)
rrcov:::.myellipse(faCov7@scores, xcov = cfaCov, main = "Robust (OGK)",
                 xlab = "Factor1", ylab = "Factor2", xlim = c(-10,10), ylim = c(-10,10), id.n = 0)
abline(v = 0)
abline(h = 0)
par(usr)


## ----label=stock611_vs_ZoomIn_4_8, echo=FALSE, results='hide', include=FALSE----
## (4) vs (8)
usr <- par(mfrow = c(1,2))
cfaClassic <- list(center = c(0,0), cov = diag(faClassic4@eigenvalues[1:2]), n.obs = faClassic4@n.obs)
rrcov:::.myellipse(faClassic4@scores, xcov = cfaClassic, main = "Classical",
                 xlab = "Factor1", ylab = "Factor2", xlim = c(-10,10), ylim = c(-10,10), id.n = 0)
abline(v = 0)
abline(h = 0)
cfaCov <- list(center = c(0,0), cov = diag(faCov8@eigenvalues[1:2]), n.obs = faCov8@n.obs)
rrcov:::.myellipse(faCov8@scores, xcov = cfaCov, main = "Robust (OGK)",
                 xlab = "Factor1", ylab = "Factor2", xlim = c(-10,10), ylim = c(-10,10), id.n = 0)
abline(v = 0)
abline(h = 0)
par(usr)


## ----label=stock611_myplotDD, echo=FALSE, results='hide', include=FALSE----
covOgk= rrcov::CovRobust(x = scale(stock611[,3:12]), control = "ogk"); covOgk
result = myplotDD(x = covOgk)

## ----label=cutoff-id.n-sort.y-indC, echo=FALSE----------------------
cat("cutoff =", result$cutoff, "\n")
cat("id.n <- length(which(rd>cutoff))\n")
cat("id.n =", result$id.n, "\n")
cat("Here y is the robust distance (rd).\n")

Lst = list(x = result$sort.y$x[c(1:5, 607:611)], ix = result$sort.y$ix[c(1:5, 607:611)])
cat("sort.y = (To save space, only the smallest five and largest five
elements of sort.y$x and sort.y$ix are shown.)\n"); show(Lst)
cat("ind =\n"); show(result$ind)

## ----label=faClassic4-----------------------------------------------
## (4) classical, x = scale(stock611[,3:12]), cor = TRUE (correlation matrix)
faClassic4 = FaClassic(x = scale(stock611[,3:12]), factors = 2, cor = TRUE,
                       method = "pca", scoresMethod = "regression"); str(faClassic4)
summary(faClassic4)

## ----label=faCov8---------------------------------------------------
## (8) robust, x = scale(stock611[,3:12]), cor = TRUE (correlation matrix)
faCov8 = FaCov(x = scale(stock611[,3:12]), factors = 2, cor = TRUE, method = "pca",
               scoresMethod = "regression", cov.control = rrcov::CovControlOgk()); str(faCov8)
summary(faCov8)

## ----label=stock611_factorScore_4_8, echo=FALSE, results='hide', include=FALSE----
## Display scores and ordered scores.
faClassic4@scores[, 2:1]
faCov8@scores[, 1:2]

## plot the factor scores
usr <- par(mfrow=c(1,2))
plot(faClassic4, which = "factorScore", choices = 2:1)
plot(faCov8, which = "factorScore", choices = 1:2)
par(usr)


## ----label=fsOrder-orderedFsC-orderedFsOgk, results='hide'----------
orderedFsC = fsOrder(faClassic4@scores[,2:1]); orderedFsC

## ----label=fsOrder-orderedFsC-orderedFsOgk2-------------------------
Lst=list(orderedFsC[[1]][1:10,], orderedFsC[[2]][1:10,]); Lst

## ----label=fsOrder-orderedFsC-orderedFsOgk3, results='hide'---------
orderedFsOgk = fsOrder(faCov8@scores[,1:2]); orderedFsOgk

## ----label=fsOrder-orderedFsC-orderedFsOgk4-------------------------
Lst=list(orderedFsOgk[[1]][1:10,], orderedFsOgk[[2]][1:10,]); Lst

