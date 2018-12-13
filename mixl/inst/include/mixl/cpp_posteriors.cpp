// [[Rcpp::plugins(cpp11)]]        

#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
NumericMatrix posteriors(NumericVector betas, NumericMatrix probabilities,
                         int Nindividuals,
                         const NumericMatrix draws, int nDraws) {
  
  int numRndVars = 1; //!===num_rnd_vars===!;
  NumericMatrix posteriors(Nindividuals, numRndVars);
  
  for (int i=0; i < Nindividuals; i++) {
    NumericMatrix indiv_B_means(Nindividuals, numRndVars);
    NumericVector indiv_L_mean(Nindividuals);
    for (int d=0; d<nDraws; d++) {
      int draw_index = i * nDraws + d; 
      NumericMatrix::ConstRow draw = draws(draw_index, _);
      
      int rnd_idx = 0;
   //   !===rnd_declarations===!ASC_A 	+ draw_1 * @SIGMA_A1 		+ draw_7 * @SIGMA_A2;
      // indiv_B_means(i, rnd_idx++) += log(probabilities(i,d) * ASC_ptav + draw[0] * SIGMA_ptav + draw[1] * SIGMA_av);

      indiv_B_means(i, rnd_idx++) += probabilities(i,d) *
        (betas["ASC_A"] + draw[0] * betas["SIGMA_A1"] + draw[1] * betas["SIGMA_A2"]);

      indiv_L_mean(i) += probabilities(i,d);
      
    }
    
    for (int r=0; r < numRndVars; r++) {
      posteriors(i,r) += indiv_B_means(i,r) / indiv_L_mean(i); //Don't need to use means as they cancel out.
    }
  }
  return posteriors;
}


