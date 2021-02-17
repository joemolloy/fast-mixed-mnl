#include <Rcpp.h>

!===MIXED_MNL===!

#ifdef _OPENMP
  #include <omp.h>
#endif
#include "mixl/utility_function.h"

using Rcpp::DataFrame;
using Rcpp::NumericMatrix;
using Rcpp::NumericVector;
using Rcpp::CharacterVector;
using Rcpp::_;
using Rcpp::stop;

// [[Rcpp::export]]
NumericMatrix predict(NumericVector betas, DataFrame data,
                      int Nindividuals, NumericMatrix availabilities,
                      Nullable<NumericMatrix> nullableDraws, int nDraws, int num_threads=1) {
  
#ifdef _OPENMP
  omp_set_num_threads(num_threads);
#endif
  
  const int PRE_COLS = 4;
  
  int nCols = PRE_COLS + !===utility_length===!;
  NumericMatrix P(data.nrows(), nCols);
  
  CharacterVector colnames1(nCols);
  colnames1[0] = "i";
  colnames1[1] = "ID";
  colnames1[2] = "choice_index";
  colnames1[3] = "p_choice";
  for (int i=0; i < !===utility_length===!; i++) {
    
    std::ostringstream oss;
    oss << "p_" << i + 1;
    
    colnames1[PRE_COLS+i] = oss.str();
  }
  
  colnames(P) = colnames1;
  
  NumericMatrix draws;
  if (nullableDraws.isNotNull()) {
    draws = nullableDraws.get();
  }
  
  UF_args v(data, Nindividuals, availabilities, draws, nDraws, NULL, P, false);
  
  std::fill(v.P.begin(), v.P.end(), 0);
  
  if (!(v.data.containsElementNamed("ID") && v.data.containsElementNamed("CHOICE"))) {
    stop("Both ID and CHOICE columns need to be present in the data");
  }
  
  //delcare the variables that you will be using from the dataframe
  const NumericVector row_ids = v.data["ID"];
  const NumericVector choice = v.data["CHOICE"];
  
  NumericVector count;
  
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
        
      NumericMatrix::ConstRow  choices_avail = v.availabilities( i , _ );
      
      for (unsigned k=0; k < utilities.size(); ++k) {
        utilities[k] = utilities[k] * choices_avail[k];
      }
      
      double sum_utilities = utilities.sum();
      std::valarray<double> probabilities = (utilities / sum_utilities);
      
      P(i, 0) = i;
      P(i, 1) = individual_index;
      P(i, 2) = choice[i];
      P(i, 3) += probabilities[choice[i]-1];
      for (unsigned k=0; k < utilities.size(); ++k) {
        P(i, PRE_COLS + k) += probabilities[k];
      } 
      
    }
    
    P(i, 3) /= v.nDraws;
    for (unsigned k=0; k < utilities.size(); ++k) {
      P(i, PRE_COLS + k) /= v.nDraws;
    } 
    
  }
  
}
return (P);
}

