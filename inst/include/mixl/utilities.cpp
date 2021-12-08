#include <Rcpp.h>

!===MIXED_MNL===!
  

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
NumericMatrix utilities(NumericVector betas, 
                     DataFrame data,
                     int Nindividuals,
                     NumericMatrix availabilities,
                     Nullable<NumericMatrix> nullableDraws, int nDraws,
                     int num_threads=1) {
  
#ifdef _OPENMP
  omp_set_num_threads(num_threads);
#endif  
  
  NumericMatrix draws;
  if (nullableDraws.isNotNull()) {
    draws = nullableDraws.get();
  }
  
  NumericMatrix P;
  
  UF_args v(data, Nindividuals, availabilities, draws, nDraws, NULL, P, false);
  
  int pre_cols = 1 + (v.nDraws > 1); //don't have a draws column if there is only one draw
  
  NumericMatrix allUtilities(v.data.nrows() * nDraws, pre_cols + !===utility_length===!);
  
  std::fill(allUtilities.begin(), allUtilities.end(), 0);
  
  if (!(v.data.containsElementNamed("ID") && v.data.containsElementNamed("CHOICE"))) {
    stop("Both ID and CHOICE columns need to be present in the data");
  }
  
  //delcare the variables that you will be using from the dataframe
  const NumericVector row_ids = v.data["ID"];

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
      for (int d=0; d<v.nDraws; d++) {
        
        #ifdef _MIXED_MNL
              int draw_index = individual_index * v.nDraws + d; //drawsrep give the index of the draw, based on id, which we dont want to carry in here.
              NumericMatrix::ConstRow draw = v.draws(draw_index, _);
        #endif
      
        /////////////////////////
        
        !===draw_and_utility_declarations===!
          
        /////////////////////////
        
        int id = i * v.nDraws + d;
        
        allUtilities(id, 0) = i;
        if (v.nDraws > 1) {
          allUtilities(id, 1) = d;
        }
        
        NumericMatrix::ConstRow  choices_avail = v.availabilities( i , _ );
        
        for (unsigned k=0; k < utilities.size(); ++k) {
          utilities[k] = utilities[k] * choices_avail[k];
          utilities[k] = std::min(700.0, std::max(-700.0, utilities[k])); //trip utilities to +- 700 for compuational reasons
          allUtilities(id, pre_cols + k) += utilities[k];
        } 
        
      }
    }
  }
  
  return allUtilities;
}
