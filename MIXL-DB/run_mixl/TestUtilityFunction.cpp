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
NumericVector individualLL(NumericVector beta, //TODO const things!
                       DataFrame data,
                       int Nindividuals,
                       IntegerMatrix availabilities,
                       NumericMatrix draws,
                       int Ndraws,
                       NumericMatrix P) {
  v::beta = beta;
  v::data = data;
  v::Nindividuals = Nindividuals;
  v::availabilities = availabilities;
  v::draws = draws;
  v::Ndraws = Ndraws;
  v::P = P;
  v::LL = NumericVector(Nindividuals);

  NumericVector LL = runUtilityFunction();

  return LL;
}

void utilityFunction() {

}
