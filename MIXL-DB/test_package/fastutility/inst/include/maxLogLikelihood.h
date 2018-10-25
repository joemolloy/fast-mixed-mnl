// [[Rcpp::depends(RcppEigen)]]

#include <RcppEigen.h>

/*
*
* notes : on one iteration, same LL for each person is given with R code and c++ code
*
* open mp notes
* use const for inputs
* armadillo datatypes for multicore
* specifying number of cores needed?
*
* for maxLE
* - optextras::grfwd - forward difference numerical gradient approximation
* - L-BFGS algorithm + solver
* - https://yixuan.cos.name/LBFGSpp/
* https://github.com/PatWie/CppNumericalSolvers
* /
*/



#include <iostream>
#include "CppNumericalSolvers/include/cppoptlib/meta.h"
#include "CppNumericalSolvers/include/cppoptlib/problem.h"
#include "CppNumericalSolvers/include/cppoptlib/solver/bfgssolver.h"



//   BfgsSolver<MixedLogit<UF>> solver;
//TODO: MixedLogit takes all the important items. We then use a Namespace to make the variables available to the utility function
//can use macros to replace utility function names with vector/matrix access and wrap utility function in boilerplate?




// nolintnextline
using namespace cppoptlib;
using namespace Rcpp;

namespace v {
  NumericVector beta;
  DataFrame data;
  int Nindividuals;
  IntegerMatrix availabilities;
  NumericMatrix draws;
  int Ndraws;
  NumericMatrix P;

  NumericVector LL(Nindividuals);

} ;

// we define a new problem for optimizing the Simple function
// we use a templated-class rather than "auto"-lambda function for a clean architecture
template<typename UF>
class MixedLogit : public Problem<double> {
  public:
    using typename Problem<double>::TVector;

    // this is just the objective (NOT optional)
    double value(const TVector &betas) override {
      UF utilityFunction; //create a function object with all variables specified already

      //double loglikSum = loglikeSum();
      return utilityFunction.calculate(betas); //LL - but do we need to return all LL values? - separate gradient function that uses all LL values
      //return 5*x[0]*x[0] + 100*x[1]*x[1]+5;
    }

  // if you calculated the derivative by hand
  // you can implement it here (OPTIONAL)
  // otherwise it will fall back to (bad) numerical finite differences
 // void gradient(const TVector &x, TVector &grad) {
//    grad[0]  = 2*5*x[0];
//    grad[1]  = 2*100*x[1];
//  }

};

template <class MyUtilityFunction>
int fastmaxlik(MixedLogit<MyUtilityFunction> f) {

  BfgsSolver<MixedLogit<MyUtilityFunction>> solver;

  using Eigen::Map;
  using Eigen::VectorXd;
  using Rcpp::as;


  const Map<VectorXd> eigenbeta(as<Map<VectorXd> >(v::beta));
  Eigen::VectorXd eigen22 = eigenbeta;

  solver.minimize(f, eigen22);
  Rcpp::Rcout << "f in argmin " << f.value(eigen22) << std::endl;
  Rcpp::Rcout << "Solver status: " << solver.status() << std::endl;
  Rcpp::Rcout << "Final criteria values: " << std::endl << solver.criteria() << std::endl;
  return 0;
}

template<typename T> class UtilityFunction;

// [[Rcpp::export]]
int runUtilityFunction(NumericVector beta, //TODO const things!
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

  MixedLogit<UtilityFunction<Problem<double>::TVector>> f;

  return fastmaxlik(f);
}


