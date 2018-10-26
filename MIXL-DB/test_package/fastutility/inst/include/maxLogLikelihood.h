// [[Rcpp::depends(RcppEigen)]]

#include <RcppEigen.h>

using namespace Rcpp;

namespace v {
  NumericVector beta1;
  DataFrame data;
  int Nindividuals;
  IntegerMatrix availabilities;
  NumericMatrix draws;
  int Ndraws;
  NumericMatrix P;

  NumericVector LL(Nindividuals);

} ;

void utilityFunction();

NumericVector runUtilityFunction() {

  using namespace v;
  utilityFunction();

  NumericVector LL(Nindividuals);
  double logNdraws = log(Ndraws);

 //TODO: parallelise this as well
  for (int i=0; i<Nindividuals; ++i) {
    double s = 0;
    for (int draw=0; draw<Ndraws; ++draw) {
      s += exp(P(i,draw));
    }

    LL[i] = log(s)-logNdraws;

  }

  return LL;

}



