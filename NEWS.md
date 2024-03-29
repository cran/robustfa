# 2023-03-22  Frederic Bertrand <frederic.bertrand@utt.fr>
    * version 1.1-0
    * fix code to have the package back on CRAN
    * fix code to comply with CRAN remarks during the submission process
    
# 2013-11-09  Ying-Ying Zhang (Robert) <robertzhangyying@qq.com>
    * version 1.0-5
    * in util.R: 
        1) computeScores(): 
			add res$correlation = correlation
			add res$reducedCorrelation = NULL
			change res$eigenvalues = eigen(S)$values
    * in factorScorePca.R: 
		1) add res$reducedCorrelation = NULL,
		2) change res$eigenvalues = eigen(S)$values
    * in factorScorePfa.R: 
	    1) res$covariance = covariance
        2) res$correlation = correlation
		3) res$reducedCorrelation = reducedCorrelation
		4) change res$eigenvalues = eigen(S)$values
	* in AllClasses.R:
	    1) add slots for Fa: cor, reducedCorrelation
    * in FaClassic.R and FaCov.R
	    1) add slot: cor, reducedCorrelation
    * in Fa.R
		1) revise method: summary
	
# 2013-10-15  Ying-Ying Zhang (Robert) <robertzhangyying@qq.com>
    * version 1.0-4
    * large revision
    * in util.R: 
        1) add compute_cov_cor()
        2) revise computeScores()
    * in factorScorePca.R and factorScorePfa.R: 
	1) add a variable scaledX
	2) change the formula of computation of F
        3) add cor argument
        4) add the following components of return value: covariance, usedMatrix
    * in FaClassic.R and FaCov.R
	1) add cor argument
	2) add the following codes:
          if(scoresMethod != "none" && method == "mle")
	      out <- computeScores(out, x = data, covmat = covmat, cor = cor, scoresMethod = scoresMethod)
	3) add slots: covariance, usedMatrix
    * in AllClasses.R
	1) add slots for Fa: covariance, usedMatrix
    * in Fa.R 
	1) revise getFa()
	2) revise summary()
	3) revise predict()

# 2012-06-21  Ying-Ying Zhang (Robert) <robertzhangyying@qq.com>
    * version 1.0-03
    * DESCRIPTION: 
        1) Depends: methods, robustbase, pcaPP, mvtnorm, rrcov, R (>= 2.15.0)
           Because the most recent robustfa 1.0-02 update does not work with R-oldrelease (i.e. R-2.14.2).

# 2012-06-16  Ying-Ying Zhang (Robert) <robertzhangyying@qq.com>
    * version 1.0-02
    * DESCRIPTION: 
        1) Depends: methods, robustbase, pcaPP, mvtnorm, rrcov
        2) Imports: stats4, stats
        3) Suggests: grid, lattice, cluster, mclust, MASS, ellipse
    * NAMESPACE:
        1) importFrom(stats4, plot, summary, show)
        2) importFrom(rrcov, getCenter, getEigenvalues, getLoadings, getQuan, getScores, getSdev)

# 2012-05-10  Ying-Ying Zhang (Robert) <robertzhangyying@qq.com>
    * version 1.0-01
    * in AllClasses.R: 
        1) add Fa@: communality, scoringCoef, meanF, corF
    * in FaClassic.R: 
	1) add res@: communality, scoringCoef, meanF, corF
	2) change scores to outComputeScores
    * in FaCov.R: 
	1) add res@: communality, scoringCoef, meanF, corF
	2) change scores to outComputeScores
    * in factorScorePfa.R: 
        1) change res$correlation
        2) change res$eigenvalues
        3) add correlation=R
    * in utils.R: 
        1) change the function computeScores() a lot
    * computeScores.Rd:
        1) revise \value{}
        2) revise \examples{}