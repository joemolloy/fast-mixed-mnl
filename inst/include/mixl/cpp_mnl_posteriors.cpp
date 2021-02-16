#include <Rcpp.h>
//posteriors function for mnl models without random draws

using Rcpp::DataFrame;
using Rcpp::NumericMatrix;
using Rcpp::NumericVector;
using Rcpp::CharacterVector;
using Rcpp::_;
using Rcpp::stop;

// [[Rcpp::export]]
NumericMatrix mixl_posteriors(NumericVector betas, NumericMatrix probabilities,
                              int Nindividuals,DataFrame data) {
  
  int numRndVars = !===num_rnd_vars===!;
  NumericMatrix posteriors(Nindividuals, numRndVars);
  
  CharacterVector colnames1(numRndVars);
  int i=0;
  !===col_names===!  
    colnames(posteriors) = colnames1;
  
  //betas
  !===beta_declarations===!
  
  !===data_declarations===!
    
    
    NumericMatrix indiv_B_means(Nindividuals, numRndVars);
    NumericVector indiv_L_mean(Nindividuals);
    
    for (int i=0; i < Nindividuals; i++) {

      int d=0;
      int rnd_idx = 0;
        
      !===random_paramters===!
          
      indiv_L_mean(i) += probabilities(i,d);

      
      for (int r=0; r < numRndVars; r++) {
        posteriors(i,r) += indiv_B_means(i,r) / indiv_L_mean(i); //Don't need to use means as they cancel out.
      }
    }
    return posteriors;
}


