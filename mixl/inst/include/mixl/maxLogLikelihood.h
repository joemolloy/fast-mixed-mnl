#include <RcppEigen.h>
#include <nloptrAPI.h>

using namespace Rcpp;

struct UF_args {
  DataFrame data;
  int Nindividuals;
  IntegerMatrix availabilities;
  NumericMatrix draws;
  int Ndraws;
  NumericMatrix P;
  StringVector beta_names;

} ;

void utilityFunction(NumericVector betas);

NumericVector runUtilityFunction(NumericVector betas, UF_args& v) {

  NumericVector LL(v.Nindividuals);
  std::fill(v.P.begin(), v.P.end(), 0);
  
  utilityFunction(betas);

  double logNdraws = log(v.Ndraws);

 //TODO: parallelise this as well
  for (int i=0; i<v.Nindividuals; i++) {
    double s = 0;
    for (int draw=0; draw<v.Ndraws; draw++) {
      s += exp(v.P(i,draw));
 
      if (i == 8) {
        printf("%f, %f, %f\n", v.P(i,draw), log(s), logNdraws);
      }
      
    }

    LL[i] = log(s)-logNdraws;

  }

  return LL;

}


static int fcount = 0;

using namespace Rcpp;
using namespace Eigen;

typedef Map<const VectorXd>  ConstVectorT ;
typedef Map<VectorXd>  VectorT ;
typedef Eigen::Ref<const VectorXd> RefVec;

double value(const RefVec  x, UF_args& v) {
  NumericVector xx = wrap(x);
  xx.names() = v.beta_names;
  
  
  NumericVector ll = runUtilityFunction(xx, v);
  double llsum = std::accumulate(ll.begin(), ll.end(), 0.0);
  
  return llsum;
}


double myfunc(unsigned n, const double *x, double *grad, void *my_func_data) {
  
  UF_args& v = (UF_args&) my_func_data; 
  
  fcount++;
  
  ConstVectorT xx (x, n);
  VectorT gg (grad, n);
  
  const double eps = 1e-06;
  
  if (grad) {
    
    VectorXd xx1 = xx;
    VectorXd xx2 = xx;
    
    for (unsigned i = 0; i < n; i++) {
      grad[i] = 0;
      {
        double temp1 = xx1[i];
        double temp2 = xx2[i];
        xx1[i] -= eps/2;
        xx2[i] += eps/2;
        grad[i] += (value(xx2, v) - value(xx1, v))/eps;
        xx1[i] = temp1;
        xx2[i] = temp2;
      }
    }
    
  }
  
  double val = value(xx, v);
  
  Rcpp::Rcout << "iteration: " << fcount << std::setprecision(8) << val << std::endl;  
  
  return val;
}




