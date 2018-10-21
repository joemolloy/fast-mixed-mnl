#include <Rcpp.h>

template <typename F>
class UtilityFunction<F>() {

  using namespace variables;

  void define();

  double calculate();

}
