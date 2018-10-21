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

namespace variables {
  NumericVector beta;
  DataFrame data;
  int Nindividuals;
  IntegerMatrix availabilities;
  NumericMatrix draws;
  int Ndraws;
  NumericMatrix P;

  NumericVector LL(Nindividuals);

} ;


template <typename T>
class TestUtilityFunction {
  public:
    double calculate(const T &betas) {
      return 1;
    }
};


// we define a new problem for optimizing the Simple function
// we use a templated-class rather than "auto"-lambda function for a clean architecture
//template<typename UF>
class MixedLogit : public Problem<double> {
  public:
    using typename Problem<double>::TVector;

    // this is just the objective (NOT optional)

    double value(const TVector &betas) override {
      TestUtilityFunction<TVector> utilityFunction; //create a function object with all variables specified already
      utilityFunction.calculate(betas);
      //double loglikSum = loglikeSum();
      return 0; //LL - but do we need to return all LL values? - separate gradient function that uses all LL values
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

// [[Rcpp::depends(RcppEigen)]]

//template <typename UF>
//' @export fastmaxlik

// [[Rcpp::export]]
int fastmaxlik(NumericVector beta, //TODO const things!
               DataFrame data,
               int Nindividuals,
               IntegerMatrix availabilities,
               NumericMatrix draws,
               int Ndraws,
               NumericMatrix P

) {

  /*  variables::data = data;
  variables::Nindividuals = Nindividuals;
  variables::availabilities = availabilities;
  variables::draws = draws;
  variables::Ndraws = Ndraws;
  variables::P = P;
  variables::LL = NumericVector(Nindividuals);

  */
  MixedLogit f;

  BfgsSolver<MixedLogit> solver;

  using Eigen::Map;
  using Eigen::VectorXd;
  using Rcpp::as;


  const Map<VectorXd> eigenbeta(as<Map<VectorXd> >(beta));
  Eigen::VectorXd eigen22 = eigenbeta;

  Eigen::VectorXd eigen2(10); eigen2 << -1, 2;


  solver.minimize(f, eigen2);
  Rcpp::Rcout << "f in argmin " << f.value(eigen2) << std::endl;
  Rcpp::Rcout << "Solver status: " << solver.status() << std::endl;
  Rcpp::Rcout << "Final criteria values: " << std::endl << solver.criteria() << std::endl;
  return 0;
}




