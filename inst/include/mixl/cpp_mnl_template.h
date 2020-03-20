
// [[Rcpp::plugins(cpp11)]]        

#include <Rcpp.h>

#ifdef _OPENMP
  #include <omp.h>
#endif
#include "mixl/utility_function.h"


using Rcpp::DataFrame;
using Rcpp::NumericMatrix;
using Rcpp::NumericVector;
using Rcpp::_;
using Rcpp::stop;

// [[Rcpp::export]]
NumericVector logLik(NumericVector betas, //TODO const things!
                     DataFrame data,
                     int Nindividuals,
                     NumericMatrix availabilities,
                     NumericMatrix P, int num_threads=1) {
  
  #ifdef _OPENMP
    omp_set_num_threads(num_threads);
  #endif  
  
  UF_args v(data, Nindividuals, availabilities, P);
  
  NumericVector LL(v.Nindividuals);
  std::fill(v.P.begin(), v.P.end(), 0);
  
  //Rcpp::Rcout << "running utility function"<<  std::endl;
  //double begin = omp_get_wtime();
  
  utilityFunction(betas, v);
  
  //TODO: parallelise this as well
  //pragma omp parallel for
  for (int i=0; i<v.Nindividuals; i++) {
    double s = exp(v.P(i,0));
    LL[i] = log(s);
    
  }
  
  //double end = omp_get_wtime();
  //double elapsed_secs = double(end - begin) * 1000;
  /*
#pragma omp parallel
{
#pragma omp single
  Rcpp::Rcout << std::setprecision(3) << elapsed_secs << " ms on " << omp_get_num_threads() << " threads" << std::endl;
}
*/

return LL;
}

//idea - preprocess the c++ code to declare all the variables at compilation time.
//or - through r, check the names in the utility function, that they are in the data, and return error if not. Then desugarise and compile
//need to distinquish between betas, random-coeefs and parameters

void utilityFunction(NumericVector betas, UF_args& v)
{
  if (!(v.data.containsElementNamed("ID") && v.data.containsElementNamed("CHOICE"))) {
    stop("Both ID and CHOICE columns need to be present in the data");
  }
  
  
  //delcare the variables that you will be using from the dataframe
  const NumericVector row_ids = v.data["ID"];
  const NumericVector choice = v.data["CHOICE"];

  /////////////////////////////////////
  
  
  //betas
  !===beta_declarations===!
    
  
  //data
  !===data_declarations===!
    
  
  
  
  /////////////////////////////////////
  
#pragma omp parallel
{
  std::vector<double> utilities(!===utility_length===!);  //specify here the number of alternatives
  
#pragma omp for
  for (int i=0; i < v.data.nrows(); i++) {
    
    int individual_index = row_ids[i]-1; //indexes should be for c, ie. start at 0
    //Rcpp::Rcout << "indv: " << individual_index << std::endl;
      
    std::fill(utilities.begin(), utilities.end(), 0.0);
    
    /////////////////////////
    
    !===draw_and_utility_declarations===!
  
    /////////////////////////
    
    //dont edit beflow this line
    for (unsigned k=0; k < utilities.size(); ++k) {
      utilities[k] = std::min(700.0, std::max(-700.0, utilities[k])); //trip utilities to +- 700 for compuational reasons
      utilities[k] = exp(utilities[k]); //take the exponential of each utility
    }
    
    
    
    double chosen_utility = utilities[choice[i]-1]; //this -1 is needed if the choices start at 1 (as they should)
    
    NumericMatrix::ConstRow  choices_avail = v.availabilities( i , _ );
    double sum_utilities = std::inner_product(utilities.begin(), utilities.end(), choices_avail.begin(), 0.0);
    
    
    double log_p_choice = log(chosen_utility / sum_utilities);
    
      
    #pragma omp atomic 
    v.P(individual_index, 0) += log_p_choice; //sum up the draws as we go along.
      
      //   printf("Hello world from omp thread %d - %d %d | %d %d\n", tid, i, draw, individual_index, draw_index);
      
  }
    
}

}
