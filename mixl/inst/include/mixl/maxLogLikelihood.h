#include <RcppEigen.h>
#include <nloptrAPI.h>

using namespace Rcpp;

namespace v {
  DataFrame data;
  int Nindividuals;
  IntegerMatrix availabilities;
  NumericMatrix draws;
  int Ndraws;
  NumericMatrix P;
  StringVector beta_names;
  NumericVector LL(Nindividuals);

} ;

void utilityFunction(NumericVector betas);

NumericVector runUtilityFunction(NumericVector betas) {
  using namespace v;
  
  std::fill(LL.begin(), LL.end(), 8);

  utilityFunction(betas);

  double logNdraws = log(Ndraws);

 //TODO: parallelise this as well
  for (int i=0; i<Nindividuals; i++) {
    double s = 0;
    for (int draw=0; draw<Ndraws; draw++) {
      s += exp(P(i,draw));
    }

    LL[i] = log(s)-logNdraws;

  }

  return LL;

}


static int fcount = 0;

using namespace Rcpp;
using namespace Eigen;

using namespace v;


typedef Map<const VectorXd>  ConstVectorT ;
typedef Map<VectorXd>  VectorT ;
typedef Eigen::Ref<const VectorXd> RefVec;

double value(const RefVec  x) {
  NumericVector xx = wrap(x);
  xx.names() = beta_names;
  
  
  NumericVector v = runUtilityFunction(xx);
  double llsum = std::accumulate(v.begin(), v.end(), 0.0);
  
  return llsum;
}


double myfunc(unsigned n, const double *x, double *grad, void *my_func_data) {
  fcount++;
  
  ConstVectorT xx (x, n);
  VectorT gg (grad, n);
  
  const double eps = 2e-06;
  
  if (grad) {
    
    VectorXd xx1 = xx;
    VectorXd xx2 = xx;
    
    for (unsigned i = 0; i < n; i++) {
      grad[i] = 0;
      {
        double temp1 = xx1[i];
        double temp2 = xx2[i];
        xx1[i] -= eps;
        xx2[i] += eps;
        grad[i] += (value(xx2) - value(xx1))/(2*eps);
        xx1[i] = temp1;
        xx2[i] = temp2;
      }
    }
    
  }
  
  double val = value(xx);
  
  Rcpp::Rcout << "iteration: " << fcount << std::setprecision(8) << val << std::endl;  
  
  return val;
}




