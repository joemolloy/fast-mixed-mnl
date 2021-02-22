#include <Rcpp.h>

!===MIXED_MNL===!

#ifdef _OPENMP
  #include <omp.h>
#endif
#include "mixl/utility_function.h"

using Rcpp::RObject;
using Rcpp::DataFrame;
using Rcpp::NumericMatrix;
using Rcpp::NumericVector;
using Rcpp::CharacterVector;
using Rcpp::_;
using Rcpp::stop;

// [[Rcpp::export]]
NumericVector logLik(NumericVector betas, DataFrame data,
                     int Nindividuals, NumericMatrix availabilities,
                     Nullable<NumericMatrix> nullableDraws, int nDraws,
                     NumericMatrix P, NumericVector weights, int num_threads=1, bool p_indices=false) {
  
  #ifdef _OPENMP
    omp_set_num_threads(num_threads);
  #endif  
    
  NumericMatrix draws;
  if (nullableDraws.isNotNull()) {
    draws = nullableDraws.get();
  }
    
  UF_args v(data, Nindividuals, availabilities, draws, nDraws, weights, P, p_indices);
  
  NumericVector LL(v.Nindividuals);
  std::fill(v.P.begin(), v.P.end(), 0);
  
  //double begin = omp_get_wtime();
  
  utilityFunction(betas, v);
  
  const double lognDraws = log(v.nDraws);
  
  //TODO: parallelise this as well
  //pragma omp parallel for
  for (int i=0; i<v.Nindividuals; i++) {
    double s = 0;
    for (int draw=0; draw<v.nDraws; draw++) {
      v.P(i,draw) = exp(v.P(i,draw));
      s += v.P(i,draw);
    }
    
    LL[i] = log(s) - lognDraws;
    
  }

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
  
  NumericVector count;
  
  if (v.include_probability_indices){
    count = v.data["count"];
  }
  /////////////////////////////////////
  
  
  //betas
  !===beta_declarations===!
    
  
  //data
  !===data_declarations===!
  
  /////////////////////////////////////
  
#pragma omp parallel
{
  std::valarray<double> utilities(!===utility_length===!);  //specify here the number of alternatives
  
#pragma omp for
  for (int i=0; i < v.data.nrows(); i++) {
    
    int individual_index = row_ids[i]-1; //indexes should be for c, ie. start at 0
    //Rcpp::Rcout << "indv: " << individual_index << std::endl;
    for (int d=0; d<v.nDraws; d++) {

      #ifdef _MIXED_MNL
        int draw_index = individual_index * v.nDraws + d; //drawsrep give the index of the draw, based on id, which we dont want to carry in here.
        NumericMatrix::ConstRow draw = v.draws(draw_index, _);
      #endif
        
      std::fill(std::begin(utilities), std::end(utilities), 0.0);
      
      /////////////////////////
      
      !===draw_and_utility_declarations===!
    
      /////////////////////////
      
      //dont edit beflow this line
      for (unsigned k=0; k < utilities.size(); ++k) {
        utilities[k] = std::min(700.0, std::max(-700.0, utilities[k])); //trip utilities to +- 700 for compuational reasons
        utilities[k] = exp(utilities[k]); //take the exponential of each utility
      }
    
      double chosen_utility = utilities[choice[i]-1]; //this -1 is needed if the choices start at 1 (as they should)

      double sum_utilities = 0.0;
      NumericMatrix::ConstRow  choices_avail = v.availabilities( i , _ );
      for (unsigned k=0; k < utilities.size(); ++k) {
        sum_utilities += utilities[k] * choices_avail[k];
      }
      
      double log_p_choice = log((chosen_utility / sum_utilities))  * v.weights[i];
      
      if (v.include_probability_indices){
        
        double p_indic_total = 0;
        !===prob_indicator_sum===!
        log_p_choice += (1/count[i]) * log(p_indic_total) * v.weights[i];
      }
      
      #pragma omp atomic 
      v.P(individual_index, d) += log_p_choice; //sum up the draws as we go along.
      
      }
    }
  }
}

