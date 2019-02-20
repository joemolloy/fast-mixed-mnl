
// [[Rcpp::plugins(openmp)]]
// [[Rcpp::plugins(cpp11)]]        

#include <Rcpp.h>
#include "mixl/utility_function.h"

using Rcpp::DataFrame;
using Rcpp::NumericMatrix;
using Rcpp::NumericVector;
using Rcpp::CharacterVector;
using Rcpp::_;
using Rcpp::stop;

// [[Rcpp::export]]
NumericVector logLik(NumericVector betas, DataFrame data,
                     int Nindividuals, NumericMatrix availabilities,
                     NumericMatrix draws, int nDraws,
                     NumericMatrix P, int num_threads=1, bool p_indices=false) {
  
  omp_set_num_threads(num_threads);
  
  UF_args2 v(data, Nindividuals, availabilities, draws, nDraws, P, p_indices);
  
  NumericVector LL(v.Nindividuals);
  std::fill(v.P.begin(), v.P.end(), 0);
  
  //Rcpp::Rcout << "running utility function"<<  std::endl;
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

void utilityFunction(NumericVector betas, UF_args2& v)
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
      
      int draw_index = individual_index * v.nDraws + d; //drawsrep give the index of the draw, based on id, which we dont want to carry in here.
      NumericMatrix::ConstRow draw = v.draws(draw_index, _);
      
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
      
      double log_p_choice = log(chosen_utility / sum_utilities);
      
      if (v.include_probability_indices){
        double p_indic_total = 0;
        !===prob_indicator_sum===!
        log_p_choice += (1/count[i])*log(p_indic_total);
      }
      
      #pragma omp atomic 
      v.P(individual_index, d) += log_p_choice; //sum up the draws as we go along.
      
      }
    }
  }
}


// [[Rcpp::export]]
NumericMatrix predict(NumericVector betas, DataFrame data,
                      int Nindividuals, NumericMatrix availabilities,
                      NumericMatrix draws, int nDraws, int num_threads=1) {
  
  omp_set_num_threads(num_threads);
  
  const int PRE_COLS = 5;
  
  int nCols = PRE_COLS + !===utility_length===!;
  NumericMatrix P(data.nrows()*nDraws, nCols);
  
  CharacterVector colnames1(nCols);
  colnames1[0] = "i";
  colnames1[1] = "ID";
  colnames1[2] = "draw";
  colnames1[3] = "choice_index";
  colnames1[4] = "p_choice";
  for (int i=0; i < !===utility_length===!; i++) {
    
    std::ostringstream oss;
    oss << "p_" << i + 1;
    
    colnames1[PRE_COLS+i] = oss.str();
  }
  
  colnames(P) = colnames1;
  
  UF_args2 v(data, Nindividuals, availabilities, draws, nDraws, P, false);
  
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
        
        int draw_index = individual_index * v.nDraws + d; //drawsrep give the index of the draw, based on id, which we dont want to carry in here.
        NumericMatrix::ConstRow draw = v.draws(draw_index, _);
        
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
        
        double pchoice = chosen_utility / sum_utilities;
        std::valarray<double> probabilities = utilities / sum_utilities;
        
        int out_index = i*v.nDraws + d;
        
        P(out_index, 0) = i;
        P(out_index, 1) = individual_index;
        P(out_index, 2) = d;
        P(out_index, 3) = choice[i];
        P(out_index, 4) = pchoice;
        for (unsigned k=0; k < utilities.size(); ++k) {
          P(out_index, PRE_COLS + k) = probabilities[k];
        } 
        
      }
      
    }
    
  }
  return (P);
}

