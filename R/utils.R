detail=function(x){
res <- list(x=x,
	    isS4=isS4(x),
	    isObject=is.object(x),
	    class=class(x),
	    attributes=attributes(x))
res
}

computeScores = function (x, newdata, scoresMethod) {
    if (!is.list(x))
	stop("x is not a list")

    if (scoresMethod == "none"){
          scoringCoef=F=meanF=corF=NULL
    }
    else if (scoresMethod == "regression"){
        # 计算因子得分系数
	  scoringCoef=t(x$loadings[]) %*% solve(x$correlation)
        # 计算因子得分矩阵
          F=newdata %*% solve(x$correlation) %*% x$loadings[] 
	# 求因子得分矩阵F的样本均值
	  meanF=apply(F,2,mean)
	# 求因子得分矩阵F的样本相关矩阵
          corF=cor(F)
     }
     else{ ## (scoresMethod == "Bartlett")
	# 计算因子得分系数
  	  ADA.inv=solve(t(x$loadings[]) %*% diag(1/x$uniquenesses) %*% x$loadings[])
  	  scoringCoef=ADA.inv %*% t(x$loadings[]) %*% diag(1/x$uniquenesses)
	# 计算因子得分矩阵
  	  F=newdata %*% diag(1/x$uniquenesses) %*% x$loadings[] %*% ADA.inv
	# 求因子得分矩阵F的样本均值
	  meanF=apply(F,2,mean)
	# 求因子得分矩阵F的样本相关矩阵
          corF=cor(F)
     }

     res <- list(
		scoringCoef=scoringCoef,
		F=F,
		meanF=meanF, 
		corF=corF)
     res
}
