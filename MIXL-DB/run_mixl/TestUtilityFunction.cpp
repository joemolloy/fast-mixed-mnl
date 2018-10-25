// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::plugins(openmp)]]
// [[Rcpp::depends(RcppEigen)]]
// [[Rcpp::depends(fastutility)]]

#include <omp.h>
#include <Rcpp.h>

#include <maxLogLikelihood.h>

//RCPP_MODULE(runUtilityFunction) {
//  function("runUtilityFunction", &norm);
//}

// [[Rcpp::export]]
int runUtilityFunction(NumericVector beta, //TODO const things!
                       DataFrame data,
                       int Nindividuals,
                       IntegerMatrix availabilities,
                       NumericMatrix draws,
                       int Ndraws,
                       NumericMatrix P

);

template<typename T>
class UtilityFunction
 {
public:
  double calculate(const T &betas) {

    double val = 5*betas[0]*betas[0] + 100*betas[1]*betas[1]+5;
    double val2 = v::availabilities[0];

    return val + val2;
  }
};

