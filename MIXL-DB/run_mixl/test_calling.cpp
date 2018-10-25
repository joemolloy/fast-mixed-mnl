// [[Rcpp::depends(fastutility)]]
// [[Rcpp::depends(RcppEigen)]]


#include <Rcpp.h>
#include <fastutility.h>

using namespace Rcpp;

using namespace v;


template<typename T>
class TestUtilityFunction {
public:
  double calculate(const T &betas) {
    return std::accumulate(v::draws.begin(),  v::draws.end(), 0.0);;
  }
};



//' @export runMyUtilityFunction
// [[Rcpp::export]]
int runMyUtilityFunction(NumericVector beta, //TODO const things!
                         DataFrame data,
                         int Nindividuals,
                         IntegerMatrix availabilities,
                         NumericMatrix draws,
                         int Ndraws,
                         NumericMatrix P

) {
  v::beta = beta;
  v::data = data;
  v::Nindividuals = Nindividuals;
  v::availabilities = availabilities;
  v::draws = draws;
  v::Ndraws = Ndraws;
  v::P = P;
  v::LL = NumericVector(Nindividuals);

  MixedLogit<TestUtilityFunction<Problem<double>::TVector>> f;

  return fastmaxlik(f);
}

