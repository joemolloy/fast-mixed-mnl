// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::plugins(openmp)]]
// [[Rcpp::depends(RcppEigen)]]
// [[Rcpp::depends(mixl)]]

#include <omp.h>
#include <Rcpp.h>

#include <mixl/maxLogLikelihood.h>

using namespace Rcpp;

//idea - preprocess the c++ code to declare all the variables at compilation time.
//or - through r, check the names in the utility function, that they are in the data, and return error if not. Then desugarise and compile
//need to distinquish between betas, random-coeefs and parameters


// [[Rcpp::export]]
NumericVector individualLL(NumericVector beta, //TODO const things!
                           DataFrame data,
                           int Nindividuals,
                           IntegerMatrix availabilities,
                           NumericMatrix draws,
                           int Ndraws,
                           NumericMatrix P) {
  v::beta1 = beta;
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

void utilityFunction()
{
  using namespace v;

  //  Rcpp::Rcout << "running utility function"<<  std::endl;
  clock_t begin = std::clock();

  std::fill(P.begin(), P.end(), 0);

  //delcare the variables that you will be using from the dataframe
  const NumericVector ids = data["ID"];
  const NumericVector row_ids = data["p_row_id"];
  const NumericVector choice = data["CHOICE"];

  //betas
  !=== beta_declarations ===!

  //data
  !=== data_declarations ===!

  //for parallel, decide on private, shared varaibles
  int nthreads = omp_get_num_threads();
  printf("Number of threads = %d\n", nthreads);


  #pragma omp parallel
{
  std::array<double, 15> utilities = { {0} };  //specify here the number of alternatives

  #pragma omp for
  for (int i=0; i < data.nrows(); i++) {

      int individual_index = row_ids[i]; //indexes should be for c, ie. start at 0

      int tid = omp_get_thread_num();

      for (int d=0; d<Ndraws; d++) {

        int draw_index = individual_index * Ndraws + d; //drawsrep give the index of the draw, based on id, which we dont want to carry in here.
        NumericMatrix::Row draw = draws(draw_index, _);

        !=== draw_and_utility_declarations ===!

        //dont edit beflow this line
        for (int k=0; k < utilities.size(); ++k) {
          utilities[k] = std::min(700.0, std::max(-700.0, utilities[k])); //trip utilities to +- 700 for compuational reasons
          utilities[k] = exp(utilities[k]); //take the exponential of each utility
        }

        IntegerMatrix::Row  choices_avail = availabilities( i , _ );

        double chosen_utility = utilities[choice[i]-1]; //this -1 is needed if the choices start at 1 (as they should)
        double sum_utilities = std::inner_product(utilities.begin(), utilities.end(), choices_avail.begin(), 0.0);
        double p_choice = chosen_utility / sum_utilities;

        //TODO: have a flag here to store utilities in namespace v

  //      printf("%f, %f\n", chosen_utility, sum_utilities);

        #pragma omp atomic
        P(individual_index, d) += log(p_choice); //sum up the draws as we go along.
        
    //   printf("Hello world from omp thread %d - %d %d | %d %d\n", tid, i, draw, individual_index, draw_index);

      }

    }

}

    clock_t end = std::clock();
    double elapsed_secs = 1000.0 * double(end - begin) / CLOCKS_PER_SEC;
  Rcpp::Rcout << std::setprecision(3) << "time ms: " << elapsed_secs << std::endl;




}






