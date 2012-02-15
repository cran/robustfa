## Define class unions for optional slots, e.g. for definition
##  of slots which will be computed on demand, like the
##  mahalanobis/robust distances
setClassUnion("Unumeric", c("numeric", "NULL"))
setClassUnion("Ulogical", c("logical", "NULL"))

###################### FA ####################################
setClass("Fa", representation(call="language",
			      converged="Ulogical",
			      loadings="matrix",
			      uniquenesses="vector",
			      correlation="matrix",
			      criteria="Unumeric",
			      factors="numeric",
			      dof="Unumeric",
			      method="character",
			      scores="Umatrix",
			      scoresMethod="character",
			      STATISTIC="Unumeric",
			      PVAL="Unumeric",
			      n.obs="numeric",
			      center="Uvector",
			      eigenvalues="vector",
			      cov.control="UCovControl",
                              "VIRTUAL")) # VIRTUAL class

setClass("SummaryFa", representation(faobj = "Fa",
                                      importance  ="matrix"))

setClass("FaClassic", contains="Fa")

setClass("FaRobust", representation("VIRTUAL"),
                    contains="Fa") # VIRTUAL class

setClass("FaCov", representation(),
                    contains="FaRobust")