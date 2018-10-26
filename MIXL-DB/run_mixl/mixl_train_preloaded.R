
# clean environment

rm(list = ls())

# ------------------------------------------------------------------------------------------------------------#
#
# ------- GLOBAL SETTINGS -------
#
# ------------------------------------------------------------------------------------------------------------#

# source code

load("../checkpoint.RData")

#source("source_code.R") # estimation/postestimation
#source("texout.R")      # creates formatted latex output
# Note: Use "multitable.R" to combine different model outputs

# overall settings
library(fastutility)
  #backups incase we run on the local session
p = matrix(0, nrow=N, ncol=Ndraws)

n <- as.integer(N)

Ndraws <- as.integer(Ndraws)

#TODO: check type of N and Ndraws

if (mixing==1) {
  Ndraws <- 5
} else Ndraws <- NA

print (paste0("print number draws...............", Ndraws))

availabilities = as.matrix(data1[,(4+1):(4+15)])

#seq_r <- individualLoglikelihood(beta, data1, N, availabilities, draws, Ndraws, p)

#print (seq_r)

#print (sum(seq_r))

#print (fastmaxlik(c(1,2,3,4)))

#print (vecdSum(c(1,2,3)))


library(Rcpp)
library(fastutility)
sourceCpp(file = "TestUtilityFunction.cpp")
individualLL(beta, data1, N, availabilities, draws, Ndraws, p)


####################

compileUtilityFunction <- function( file ) {
  processed_script <- preprocess_file(file, save_location)
  sourceCpp(fnName, code = processed_script)

  return (fnName) #TODO: need to make sure the function is returned

}




runMaxLik (data, availabilities, N, beta, draws, Ndraws, p) {

  individualLL <- compileUtilityFunction() # - gives the parallel c++ utility function - maybe can precompile this? or does caching handle this already

  loglike <- function (beta) {
      LL <- individualLL (beta, data, N, availabilities, draws, Ndraws, p)
      return (LL)
  }

  model <- maxLik::maxLik (loglike,start=beta,fixed=fixedparams,method="BFGS",print.level=3,iterlim=10000)

  return (model)

}



