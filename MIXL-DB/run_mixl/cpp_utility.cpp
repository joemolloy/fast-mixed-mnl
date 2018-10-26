// [[Rcpp::plugins(cpp11)]]
// [[Rcpp::plugins(openmp)]]
// [[Rcpp::depends(RcppEigen)]]
// [[Rcpp::depends(fastutility)]]

#include <omp.h>
#include <Rcpp.h>

#include "../test_package/fastutility/inst/include/maxLogLikelihood.h"

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


  //declare the coefficients from beta1, it is faster this way???? todo: really
  //TODO: collects beta1s
  double ASC_W = v::beta1["ASC_W"];
  double ASC_B = beta1["ASC_B"];
  double ASC_C = beta1["ASC_C"];
  double ASC_CP = beta1["ASC_CP"];
  double ASC_CS = beta1["ASC_CS"];

  double B_COST = beta1["B_COST"];

  double B_TT_W = beta1["B_TT_W"];
  double B_TT_B = beta1["B_TT_B"];
  double B_TT_C = beta1["B_TT_C"];
  double B_TT_PT = beta1["B_TT_PT"];
  double B_TT_CS = beta1["B_TT_CS"];
  double B_TT_CP = beta1["B_TT_CP"];

  double S_MC = beta1["S_MC"];
  double S_RCC = beta1["S_RCC"];
  double S_RCPT = beta1["S_RCPT"];

  double SIGMA_W = beta1["SIGMA_W"];
  double SIGMA_B = beta1["SIGMA_B"];
  double SIGMA_C = beta1["SIGMA_C"];
  double SIGMA_CP = beta1["SIGMA_CP"];
  double SIGMA_CS = beta1["SIGMA_CS"];

  double SIGMA_SCALE = beta1["SIGMA_SCALE"];

  double SIGMA_TT_W = beta1["SIGMA_TT_W"];
  double SIGMA_TT_B = beta1["SIGMA_TT_B"];
  double SIGMA_TT_C = beta1["SIGMA_TT_C"];
  double SIGMA_TT_CP = beta1["SIGMA_TT_CP"];
  double SIGMA_TT_CS = beta1["SIGMA_TT_CS"];
  double SIGMA_TT_PT = beta1["SIGMA_TT_PT"];


  const NumericVector tt_w_rp = data["tt_w_rp"];
  const NumericVector tt_b_rp = data["tt_b_rp"];
  const NumericVector tt_c_rp = data["tt_c_rp"];
  const NumericVector tc_c_rp = data["tc_c_rp"];
  const NumericVector tt_pt_rp = data["tt_pt_rp"];
  const NumericVector tc_pt_rp_a1 = data["tc_pt_rp_a1"];
  const NumericVector tt_bf_mc = data["tt_bf_mc"];
  const NumericVector tt_v_mc = data["tt_v_mc"];
  const NumericVector tt_cp_mc = data["tt_cp_mc"];
  const NumericVector tt_cs_mc = data["tt_cs_mc"];
  const NumericVector c_cs_mc = data["c_cs_mc"];
  const NumericVector c_cp_mc = data["c_cp_mc"];
  const NumericVector tt_pt_mc = data["tt_pt_mc"];
  const NumericVector c_pt_mc = data["c_pt_mc"];
  const NumericVector tt_a1_rcc = data["tt_a1_rcc"];
  const NumericVector c_a1_rcc = data["c_a1_rcc"];
  const NumericVector tt_a2_rcc = data["tt_a2_rcc"];
  const NumericVector tt_a3_rcc = data["tt_a3_rcc"];
  const NumericVector c_a2_rcc = data["c_a2_rcc"];
  const NumericVector c_a3_rcc = data["c_a3_rcc"];
  const NumericVector tt_a1_rcpt = data["tt_a1_rcpt"];
  const NumericVector tt_a2_rcpt = data["tt_a2_rcpt"];
  const NumericVector tt_a3_rcpt = data["tt_a3_rcpt"];
  const NumericVector c_a1_rcpt = data["c_a1_rcpt"];
  const NumericVector c_a2_rcpt = data["c_a2_rcpt"];
  const NumericVector c_a3_rcpt = data["c_a3_rcpt"];

  //for parallel, decide on private, shared varaibles
  int nthreads = omp_get_num_threads();
  //printf("Number of threads = %d\n", nthreads);


  #pragma omp parallel
{
  std::array<double, 15> utilities = { {0} };  //specify here the number of alternatives

  #pragma omp for
  for (int i=0; i < data.nrows(); i++) {

      int individual_index = row_ids[i]; //indexes should be for c, ie. start at 0

      int tid = omp_get_thread_num();

      for (int draw=0; draw<Ndraws; draw++) {

        int draw_index = individual_index * Ndraws + draw; //drawsrep give the index of the draw, based on id, which we dont want to carry in here.
        NumericMatrix::Row dr = draws(draw_index, _);


        int draw_num = 0;
        double B_COST_RND = -exp( B_COST + dr[draw_num++] * SIGMA_SCALE ) ;

        double B_TT_W_RND  = ( B_TT_W + dr[draw_num++] * SIGMA_TT_W )  ;
        double B_TT_B_RND  = ( B_TT_B + dr[draw_num++] * SIGMA_TT_B ) ;
        double B_TT_C_RND  = ( B_TT_C + dr[draw_num++] * SIGMA_TT_C ) ;
        double B_TT_PT_RND = ( B_TT_PT + dr[draw_num++] * SIGMA_TT_PT ) ;
        double B_TT_CS_RND = ( B_TT_CS + dr[draw_num++] * SIGMA_TT_CS ) ;
        double B_TT_CP_RND = ( B_TT_CP + dr[draw_num++] * SIGMA_TT_CP ) ;

        double ASC_W_RNP  = ASC_W + dr[draw_num++] * SIGMA_W ;
        double ASC_B_RNP  = ASC_B + dr[draw_num++] * SIGMA_B ;
        double ASC_C_RNP  = ASC_C + dr[draw_num++] * SIGMA_C ;
        double ASC_CS_RNP  = ASC_CS + dr[draw_num++] * SIGMA_CS ;
        double ASC_CP_RNP  = ASC_CP + dr[draw_num++] * SIGMA_CP ;

        int u_i = 0;

        utilities[u_i++] =  1 * (ASC_W_RNP + B_COST_RND * B_TT_W_RND * tt_w_rp[i] / 60 );

        utilities[u_i++] =  1 * (ASC_B_RNP + B_COST_RND * B_TT_B_RND * tt_b_rp[i] / 60 );

        utilities[u_i++] =  1 * (ASC_C_RNP + B_COST_RND * ( B_TT_C_RND * tt_c_rp[i] / 60 +  tc_c_rp[i] ));

        utilities[u_i++] =  1 * (B_COST_RND * ( B_TT_PT_RND * tt_pt_rp[i] / 60  + tc_pt_rp_a1[i] ));

        utilities[u_i++] =  S_MC * (ASC_W_RNP  + B_COST_RND * B_TT_W_RND * tt_bf_mc[i] / 60);

        utilities[u_i++] =  S_MC * (ASC_B_RNP  + B_COST_RND * B_TT_B_RND * tt_v_mc[i] / 60);

        utilities[u_i++] =  S_MC * (ASC_CP_RNP + B_COST_RND * ( B_TT_CP_RND * tt_cp_mc[i] / 60 + c_cp_mc[i] ));

        utilities[u_i++] =  S_MC * (ASC_CS_RNP + B_COST_RND * ( B_TT_CS_RND * tt_cs_mc[i] / 60 + c_cs_mc[i] ));

        utilities[u_i++] =  S_MC *  (B_COST_RND * ( B_TT_PT_RND * tt_pt_mc[i] / 60 + c_pt_mc[i] ));

        // SP RCC data

        utilities[u_i++] =  S_RCC *  (B_COST_RND * ( B_TT_CS_RND * tt_a1_rcc[i] / 60 + c_a1_rcc[i] ));

        utilities[u_i++] =  S_RCC *  (B_COST_RND * ( B_TT_CS_RND * tt_a2_rcc[i] / 60 + c_a2_rcc[i] ));

        utilities[u_i++] =  S_RCC *  (B_COST_RND * ( B_TT_CS_RND * tt_a3_rcc[i] / 60 + c_a3_rcc[i] ));

        // SP RCPT data

        utilities[u_i++] =  S_RCPT *  (B_COST_RND * ( B_TT_PT_RND * tt_a1_rcpt[i] / 60 + c_a1_rcpt[i] ));

        utilities[u_i++] =  S_RCPT *  (B_COST_RND * ( B_TT_PT_RND * tt_a2_rcpt[i] / 60 + c_a2_rcpt[i] ));

        utilities[u_i++] =  S_RCPT *  (B_COST_RND * ( B_TT_PT_RND * tt_a3_rcpt[i] / 60 + c_a3_rcpt[i] ));


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

        #pragma omp critical
        {
                P(individual_index, draw) += log(p_choice); //sum up the draws as we go along.
        }
    //    printf("Hello world from omp thread %d - %d %d | %d %d\n", tid, i, draw, individual_index, draw_index);

      }

    }

}

    clock_t end = std::clock();
    double elapsed_secs = 1000.0 * double(end - begin) / CLOCKS_PER_SEC;
  Rcpp::Rcout << std::setprecision(3) << "time ms: " << elapsed_secs << std::endl;




}






