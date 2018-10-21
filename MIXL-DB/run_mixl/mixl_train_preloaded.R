
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

seq_r <- individualLoglikelihood(beta, data1, N, availabilities, draws, Ndraws, p)

print (seq_r)

#print (sum(seq_r))

print (fastmaxlik(c(1,2,3,4)))

print (vecdSum(c(1,2,3)))


library(Rcpp)
cppFunction('
    int fastmaxlik(NumericVector beta ) {

  using Eigen::Map;
  using Eigen::VectorXd;
  using Rcpp::as;


  const Map<VectorXd> eigenbeta(as<Map<VectorXd> >(beta));
  Eigen::VectorXd eigen22 = eigenbeta;

  Rcpp::Rcout << eigenbeta << std::endl;

  return 0;
}', depends = "RcppEigen")
