// [[Rcpp::plugins(openmp)]]
// [[Rcpp::depends(RcppEigen)]]

#include <RcppEigen.h>

using namespace Rcpp;

struct UF_args {
  const DataFrame data;
  const int Nindividuals;
  const Nullable<IntegerMatrix> availabilities;
  const NumericMatrix draws;
  const int Ndraws;
  NumericMatrix P;
  bool include_probability_indices;
  
  UF_args(DataFrame data, int Nindividuals, Nullable<IntegerMatrix> availabilities, NumericMatrix draws, int Ndraws, NumericMatrix P,bool include_probability_indices)
    : data(data), Nindividuals(Nindividuals), availabilities(availabilities), draws(draws), Ndraws(Ndraws), P(P),include_probability_indices(include_probability_indices)
  { }
  
} ;

void utilityFunction(NumericVector beta1, UF_args& v);

// [[Rcpp::export]]
NumericVector logLik(NumericVector betas, //TODO const things!
                     DataFrame data,
                     int Nindividuals,
                     Nullable<IntegerMatrix> availabilities,
                     NumericMatrix draws,
                     int Ndraws,
                     NumericMatrix P, int num_threads=1, bool p_indices=false) {
  
  omp_set_num_threads(num_threads);
  
  UF_args v(data, Nindividuals, availabilities, draws, Ndraws, P, p_indices);
  
  NumericVector LL(v.Nindividuals);
  std::fill(v.P.begin(), v.P.end(), 0);
  
  //Rcpp::Rcout << "running utility function"<<  std::endl;
  double begin = omp_get_wtime();
  
  utilityFunction(betas, v);
  
  const double logNdraws = log(v.Ndraws);
  
  //TODO: parallelise this as well
  //pragma omp parallel for
  for (int i=0; i<v.Nindividuals; i++) {
    double s = 0;
    for (int draw=0; draw<v.Ndraws; draw++) {
      s += exp(v.P(i,draw));
    }
    
    LL[i] = log(s) - logNdraws;
    
  }
  
  double end = omp_get_wtime();
  double elapsed_secs = double(end - begin) * 1000;
  
#pragma omp parallel
{
#pragma omp single
  Rcpp::Rcout << std::setprecision(3) << elapsed_secs << " ms on " << omp_get_num_threads() << " threads" << std::endl;
}


return LL;
}

//idea - preprocess the c++ code to declare all the variables at compilation time.
//or - through r, check the names in the utility function, that they are in the data, and return error if not. Then desugarise and compile
//need to distinquish between betas, random-coeefs and parameters

void utilityFunction(NumericVector beta1, UF_args& v)
{
  
  //delcare the variables that you will be using from the dataframe
  const NumericVector ids = v.data["ID"];
  const NumericVector row_ids = v.data["p_row_id"];
  const NumericVector choice = v.data["CHOICE"];
  const NumericVector count = v.data["count"];
  
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
    
    int individual_index = row_ids[i]; //indexes should be for c, ie. start at 0
    //Rcpp::Rcout << "indv: " << individual_index << std::endl;
    for (int d=0; d<v.Ndraws; d++) {
      
      int draw_index = individual_index * v.Ndraws + d; //drawsrep give the index of the draw, based on id, which we dont want to carry in here.
      NumericMatrix::ConstRow draw = v.draws(draw_index, _);
      
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
      double sum_utilities = 0;
      if (v.availabilities.isNotNull()) {
        const IntegerMatrix availabilities(v.availabilities);
        IntegerMatrix::ConstRow  choices_avail = availabilities( i , _ );
        sum_utilities = std::inner_product(utilities.begin(), utilities.end(), choices_avail.begin(), 0.0);
      }
      else {
        sum_utilities = std::accumulate(utilities.begin(), utilities.end(), 0.0);
      }
      
      double log_p_choice = log(chosen_utility / sum_utilities);
      
      if (v.include_probability_indices){
        !===prob_indicator_sum===!
        log_p_choice += (1/count[i])*log(p_indic_total);
      }
      
#pragma omp atomic 
      v.P(individual_index, d) += log_p_choice; //sum up the draws as we go along.
      
      //   printf("Hello world from omp thread %d - %d %d | %d %d\n", tid, i, draw, individual_index, draw_index);
      
    }
    
  }
  
}

}
