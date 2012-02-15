factorScorePca<-
function(x, factors=2, covmat=NULL, rotation = c("varimax", "none"), scoresMethod = c("none", "regression", "Bartlett")){
   cl <- match.call()
   
   # "factorScorePca" always uses the correlation matrix
   if (!is.null(covmat)) {
	  if (is.list(covmat)) covmat=covmat$cov
	  R=cov2cor(covmat)
   }
   else if (!is.null(x))
	R=cor(x) 
   else # covmat==NULL and x==NULL
	stop("no covmat or x provided")

   p<-nrow(R); diag_R<-diag(R); sum_rank<-sum(diag_R)
   rowname<-paste("X", 1:p, sep="")
   colname<-paste("Factor", 1:factors, sep="")
   A0<-matrix(0, nrow=p, ncol=factors, 
             dimnames=list(rowname, colname))
   eig<-eigen(R)
   for (i in 1:factors)
      A0[,i]<-sqrt(eig$values[i])*eig$vectors[,i]

   if (missing(rotation) || rotation == "varimax")
        A=varimax(A0, normalize = T)$loadings
   else if (rotation == "none")
        A=A0
   else cat("undefined rotation method, try rotation = 'varimax' or 'none' \n")
   
   # A is the factor loadings after rotation. The following for loop makes sure that 
   # in each column of A, the entry with the largest absolute value is always positive!
   # ��ʱA��ÿһ���о���ֵ����Ԫ����Ϊ����
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
        # �������ӵ÷�ϵ��
	  scoringCoef=t(A) %*% solve(R)
        # �������ӵ÷־���
        # F=scale(x, scale=F) %*% solve(R) %*% A # ���Ļ��任������
          F=scale(x) %*% solve(R) %*% A # ��׼���任
	# �����ӵ÷־���F��������ֵ
	  meanF=apply(F,2,mean)
	# �����ӵ÷־���F��������ؾ���
          corF=cor(F)
      }
   
      if (scoresMethod == "Bartlett"){
	# �������ӵ÷�ϵ��
  	  ADA.inv=solve(t(A) %*% diag(1/specific) %*% A)
  	  scoringCoef=ADA.inv %*% t(A) %*% diag(1/specific)
	# �������ӵ÷־���
  	  F=scale(x) %*% diag(1/specific) %*% A %*% ADA.inv
	# �����ӵ÷־���F��������ֵ
	  meanF=apply(F,2,mean)
	# �����ӵ÷־���F��������ؾ���
          corF=cor(F)
      }
   }

   method<-c("pca") # Principal Component Method
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
   class(res)="factorScorePca"
   res
}