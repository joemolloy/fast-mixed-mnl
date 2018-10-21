#include <Rcpp.h>
#include <typeinfo>
#include <ctime>
#include <array>

#include <RcppEigen.h>
#include <omp.h>
// [[Rcpp::plugins(openmp)]]

using namespace Rcpp;

//idea - preprocess the c++ code to declare all the variables at compilation time.
//or - through r, check the names in the utility function, that they are in the data, and return error if not. Then desugarise and compile
//need to distinquish between betas, random-coeefs and parameters



int utilityFunction(const NumericVector beta,
                    const DataFrame data,
                    const IntegerMatrix availabilities, //Nind x numchoices
                    const NumericMatrix draws,
                    const int Ndraws,
                    NumericMatrix P  )
{

  //  Rcpp::Rcout << "running utility function"<<  std::endl;
  //  clock_t begin = std::clock();

  std::fill(P.begin(), P.end(), 0);

  //delcare the variables that you will be using from the dataframe
  const NumericVector ids = data["ID"];
  const NumericVector row_ids = data["p_row_id"];
  const NumericVector choice = data["CHOICE"];


  //declare the coefficients from beta, it is faster this way???? todo: really
  //TODO: collects betas
  double ASC_W = beta["ASC_W"];
  double ASC_B = beta["ASC_B"];
  double ASC_C = beta["ASC_C"];
  double ASC_CP = beta["ASC_CP"];
  double ASC_CS = beta["ASC_CS"];

  double B_COST = beta["B_COST"];

  double B_TT_W = beta["B_TT_W"];
  double B_TT_B = beta["B_TT_B"];
  double B_TT_C = beta["B_TT_C"];
  double B_TT_PT = beta["B_TT_PT"];
  double B_TT_CS = beta["B_TT_CS"];
  double B_TT_CP = beta["B_TT_CP"];

  double S_MC = beta["S_MC"];
  double S_RCC = beta["S_RCC"];
  double S_RCPT = beta["S_RCPT"];

  double SIGMA_W = beta["SIGMA_W"];
  double SIGMA_B = beta["SIGMA_B"];
  double SIGMA_C = beta["SIGMA_C"];
  double SIGMA_CP = beta["SIGMA_CP"];
  double SIGMA_CS = beta["SIGMA_CS"];

  double SIGMA_SCALE = beta["SIGMA_SCALE"];

  double SIGMA_TT_W = beta["SIGMA_TT_W"];
  double SIGMA_TT_B = beta["SIGMA_TT_B"];
  double SIGMA_TT_C = beta["SIGMA_TT_C"];
  double SIGMA_TT_CP = beta["SIGMA_TT_CP"];
  double SIGMA_TT_CS = beta["SIGMA_TT_CS"];
  double SIGMA_TT_PT = beta["SIGMA_TT_PT"];


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
  printf("Number of threads = %d\n", nthreads);
#pragma omp parallel
{
  std::array<double, 15> utilities = { {0} };  //specify here the number of alternatives

  #pragma omp for
  for (int i=0; i < data.nrows(); i++) {

      int individual_index = row_ids[i]; //indexes should be for c, ie. start at 0

      int tid = omp_get_thread_num();

      for (int draw=0; draw<Ndraws; draw++) {

        int draw_index = individual_index * Ndraws + draw; //drawsrep give the index of the draw, based on id, which we dont want to carry in here.
        NumericMatrix::ConstRow dr = draws(draw_index, _);


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

        IntegerMatrix::ConstRow  choices_avail = availabilities( i , _ );

        double chosen_utility = utilities[choice[i]-1]; //this -1 is needed if the choices start at 1 (as they should)
        double sum_utilities = std::inner_product(utilities.begin(), utilities.end(), choices_avail.begin(), 0.0);
        double p_choice = chosen_utility / sum_utilities;

  //      printf("%f, %f\n", chosen_utility, sum_utilities);

        #pragma omp critical
        {
                P(individual_index, draw) += log(p_choice); //sum up the draws as we go along.
        }
    //    printf("Hello world from omp thread %d - %d %d | %d %d\n", tid, i, draw, individual_index, draw_index);

      }

    }

}

  //  end = std::clock();
  //  elapsed_secs = 1000.0 * double(end - begin) / CLOCKS_PER_SEC;
  //  Rcpp::Rcout << std::setprecision(2) << "time ms: " << elapsed_secs << std::endl;



  return 0;

}

//' @export loglikelihood
//' '@useDynLib fastutility, .registration = TRUE'
// [[Rcpp::export]]
double loglikelihood(NumericVector beta,
                     DataFrame data,
                     int Nindividuals,
                     IntegerMatrix availabilities, //Nind x numchoices
                     NumericMatrix draws,
                     int Ndraws,
                     NumericMatrix P) {

  utilityFunction(beta, data, availabilities, draws, Ndraws, P);

  double LL = 0;

  for (int i=0; i<Nindividuals; ++i) {
    double s = 0;
    for (int draw=0; draw<Ndraws; ++draw) {
      s += exp(P(i,draw));
    }

    LL  += log(s);

  }

  double LL2 = LL - Nindividuals*log(Ndraws);
  return LL2;

}

//' @export individualLoglikelihood
// [[Rcpp::export]]
NumericVector individualLoglikelihood(NumericVector beta,
                                      DataFrame data,
                                      int Nindividuals,
                                      IntegerMatrix availabilities, //Nind x numchoices
                                      NumericMatrix draws,
                                      int Ndraws,
                                      NumericMatrix P) {


  utilityFunction(beta, data, availabilities, draws, Ndraws, P);

  NumericVector LL(Nindividuals);
  double logNdraws = log(Ndraws);

  for (int i=0; i<Nindividuals; ++i) {
    double s = 0;
    for (int draw=0; draw<Ndraws; ++draw) {
      s += exp(P(i,draw));
    }

    LL[i] = log(s)-logNdraws;

  }

  return LL;

}






// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically
// run after the compilation.
//











