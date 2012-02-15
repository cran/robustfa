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

    F=switch(scoresMethod, 
             none=NULL,
             regression=newdata %*% solve(x$correlation) %*% x$loadings[],
             Bartlett=newdata %*% diag(1/x$uniquenesses) %*% x$loadings[] %*% solve(t(x$loadings[]) %*% diag(1/x$uniquenesses) %*% x$loadings[])
            ) 
}
