#include <Rcpp.h>

!===MIXED_MNL===!

using Rcpp::DataFrame;
using Rcpp::NumericMatrix;
using Rcpp::NumericVector;
using Rcpp::Nullable;
using Rcpp::CharacterVector;
using Rcpp::_;
using std::exp;

//posteriors function for mixed models without random draws


// [[Rcpp::export]]
NumericMatrix mixl_posteriors(NumericVector betas, NumericMatrix probabilities,
                         int Nindividuals,DataFrame data,
                         Nullable<NumericMatrix> nullableDraws, int nDraws) {
  
  int numRndVars = !===num_rnd_vars===!;
  NumericMatrix posteriors(Nindividuals, numRndVars);
  
  NumericMatrix draws;
  
  if (nullableDraws.isNotNull()) {
    draws = nullableDraws.get();
  }
  
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
    for (int d=0; d<nDraws; d++) {
      
      #ifdef _MIXED_MNL
            int draw_index = i * nDraws + d; //drawsrep give the index of the draw, based on id, which we dont want to carry in here.
            NumericMatrix::Row draw = draws(draw_index, _);
      #endif
          
      int rnd_idx = 0;

      !===random_paramters===!
      
      indiv_L_mean(i) += probabilities(i,d);
      
    }
    
    for (int r=0; r < numRndVars; r++) {
      posteriors(i,r) += indiv_B_means(i,r) / indiv_L_mean(i); //Don't need to use means as they cancel out.
    }
  }
  return posteriors;
}


