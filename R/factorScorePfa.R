factorScorePfa <-
function(x, factors=2, covmat=NULL, rotation = c("varimax", "none"), scoresMethod = c("none", "regression", "Bartlett")){
   cl <- match.call()

   # "factorScorePfa" always uses the correlation matrix
   if (!is.null(covmat)) {
	  if (is.list(covmat)) covmat=covmat$cov
	  R=cov2cor(covmat)
   }
   else if (!is.null(x))
	R=cor(x) 
   else # covmat==NULL and x==NULL
	stop("no covmat or x provided")

   d=1/diag(solve(R))

   p<-nrow(R); diag_R<-diag(R); sum_rank<-sum(diag_R)
   rowname<-paste("X", 1:p, sep="")
   colname<-paste("Factor", 1:factors, sep="")
   A0<-matrix(0, nrow=p, ncol=factors, 
             dimnames=list(rowname, colname))

   kmax=20; k<-1; h <- diag_R-d
   repeat{
      diag(R)<- h; h1<-h; eig<-eigen(R)
      for (i in 1:factors)
         A0[,i]<-sqrt(eig$values[i])*eig$vectors[,i]

      h<-diag(A0 %*% t(A0))
      if ((sqrt(sum((h-h1)^2))<1e-4)|k==kmax) break
      k<-k+1
   }

   if (missing(rotation) || rotation == "varimax")
        A=varimax(A0, normalize = T)$loadings
   else if (rotation == "none")
        A=A0
   else cat("undefined rotation method, try rotation = 'varimax' or 'none' \n")
   
   # A is the factor loadings after rotation. The following for loop makes sure that 
   # in each column of A, the entry with the largest absolute value is always positive!
   # 此时A的每一列中绝对值最大的元素总为正！
   for (i in 1:factors){
      if (A[,i][which.max(abs(A[,i]))]<0){
         A[,i]=-A[,i]
      }
   }

   h<-diag(A%*%t(A))
   specific=diag_R-h

   if (missing(scoresMethod)) scoresMethod="none"

   scoringCoef <- F <- meanF <- corF <- n.obs <- center <- NULL # the "<-" can be replaced by "="
   if (!missing(x)) {
      n.obs=nrow(x)
      center=colMeans(x)

      if (scoresMethod == "regression"){
        # 计算因子得分系数
	  scoringCoef=t(A) %*% solve(R)
        # 计算因子得分矩阵
        # F=scale(x, scale=F) %*% solve(R) %*% A # 中心化变换，不好
          F=scale(x) %*% solve(R) %*% A # 标准化变换
	# 求因子得分矩阵F的样本均值
	  meanF=apply(F,2,mean)
	# 求因子得分矩阵F的样本相关矩阵
          corF=cor(F)
      }
   
      if (scoresMethod == "Bartlett"){
	# 计算因子得分系数
  	  ADA.inv=solve(t(A) %*% diag(1/specific) %*% A)
  	  scoringCoef=ADA.inv %*% t(A) %*% diag(1/specific)
	# 计算因子得分矩阵
  	  F=scale(x) %*% diag(1/specific) %*% A %*% ADA.inv
	# 求因子得分矩阵F的样本均值
	  meanF=apply(F,2,mean)
	# 求因子得分矩阵F的样本相关矩阵
          corF=cor(F)
      }
   }

   method<-c("pfa") # Principal Factor Method
   res <- list( call=cl,
		loadings=A, 
		communality=h,
		uniquenesses=specific,
		correlation=R,
		factors=factors,
		method=method,
		scores=F,
		scoringCoef=scoringCoef,
		meanF=meanF, 
		corF=corF,
		scoresMethod=scoresMethod,
		n.obs=n.obs,
		center=center,
		eigenvalues=eig$values)
   class(res)="factorScorePfa"
   res
}
