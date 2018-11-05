#include <Rcpp.h>
#include <nloptrAPI.h>


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
  
  UF_args* v = (UF_args*) my_func_data; 
  
  fcount++;
  
  ConstVectorT xx (x, n);
  double val = value(xx, *v);
  
  VectorT gg (grad, n);
  
  const double eps =  6.055454e-06;
  
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
        grad[i] += (value(xx2, *v) - value(xx1, *v))/(2*eps);
        //   grad[i] /= v->Nindividuals;
        xx1[i] = temp1;
        xx2[i] = temp2;
      }
    }
    
  }
  
  
  Rcpp::Rcout << xx << std::endl;
  
  Rcpp::Rcout << "iteration: " << fcount << std::setprecision(8) << " = " << val << std::endl;  
  
  return val;
}


// [[Rcpp::export]]
NumericVector gradient(NumericVector start_betas, //TODO const things!
                       DataFrame data,
                       int Nindividuals,
                       IntegerMatrix availabilities,
                       NumericMatrix draws,
                       int Ndraws,
                       NumericMatrix P) {
  
  UF_args v;
  
  v.data = data;
  v.Nindividuals = Nindividuals;
  v.availabilities = availabilities;
  v.draws = draws;
  v.Ndraws = Ndraws;
  v.P = P;
  v.beta_names = start_betas.names();
  
  NumericVector grad(start_betas.size());
  
  myfunc(grad.size(), start_betas.begin(), grad.begin(), &v);
  
  return grad;
}


// [[Rcpp::export]]
NumericVector runMaxLik(NumericVector start_betas, //TODO const things!
                        DataFrame data,
                        int Nindividuals,
                        IntegerMatrix availabilities,
                        NumericMatrix draws,
                        int Ndraws,
                        NumericMatrix P) {
  
  UF_args v;
  UF_args& vref = v;
  
  v.data = data;
  v.Nindividuals = Nindividuals;
  v.availabilities = availabilities;
  v.draws = draws;
  v.Ndraws = Ndraws;
  v.P = P;
  v.beta_names = start_betas.names();
  
  
  bool verbose=true;
  nlopt_opt opt = nlopt_create(NLOPT_LD_LBFGS, start_betas.size()); 
  nlopt_set_min_objective(opt, myfunc, &vref);
  nlopt_set_xtol_rel(opt, 1e-4);
  
  NumericVector x(start_betas);
  
  double minf; 							// minimum objective value, upon return
  fcount = 0;            	    // reset counters
  
  if (nlopt_optimize(opt, x.begin(), &minf) < 0) {
    if (verbose) Rcpp::Rcout << "nlopt failed!" << std::endl;
  } else {
    if (verbose) {
      Rcpp::Rcout << std::setprecision(5)
                  << "Found minimum at f(" << x[0] << "," << x[1] << ") "
                  << "= " << std::setprecision(8) << minf
                  << " after " << fcount << " function"
                  << std::endl;
    }
  }
  nlopt_destroy(opt);
  return x;
}

//compare gradients
//ll2 <- function (beta1) sum(logLik(beta1, data, Nindividuals, availabilities, draws, Ndraws, p))
//g2 <- function (beta1) gradient(beta1, data, Nindividuals, availabilities, draws, Ndraws, p)
//maxLik::compareDerivatives(ll2, g2, t0=beta)
  
