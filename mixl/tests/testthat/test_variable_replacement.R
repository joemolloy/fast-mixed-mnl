context("Test replacement and intepretation of variables in source files")

text1 <- "//regexes:
//ignore lines starting with //

B_COST_RND = -exp( @B_COST + draw0 * @SIGMA_SCALE ) ;

ASC_C_RNP  = @ASC_C + log(draw9) * @SIGMA_C ;
ASC_CS_RNP  = @ASC_CS + draw10 * @SIGMA_CS ;
ASC_CP_RNP  = @ASC_CP + draw11 * @SIGMA_CP ;

U_1 =  1 * (ASC_W_RNP + B_COST_RND * B_TT_W_RND * $tt_w_rp / 60 );
//  U_2 =  1 * (ASC_W_RNP + B_COST_RND*B_TT_W_RND * $tt_w_rp/ 60 );

U_3 =  1 * (ASC_B_RNP + B_COST_RND * B_TT_B_RND * $tt_b_rp / 60 );
"

test_template <-  "
!=== beta_declarations ===!

!=== data_declarations ===!

!=== draw_and_utility_declarations ===!
"


expected_output = "double B_COST = beta1[\"B_COST\"];
double SIGMA_SCALE = beta1[\"SIGMA_SCALE\"];
double ASC_C = beta1[\"ASC_C\"];
double SIGMA_C = beta1[\"SIGMA_C\"];
double ASC_CS = beta1[\"ASC_CS\"];
double SIGMA_CS = beta1[\"SIGMA_CS\"];
double ASC_CP = beta1[\"ASC_CP\"];
double SIGMA_CP = beta1[\"SIGMA_CP\"];

const NumericVector data_tt_w_rp = v.data[\"tt_w_rp\"];
const NumericVector data_tt_b_rp = v.data[\"tt_b_rp\"];

//regexes:
//ignore lines starting with //

double B_COST_RND = -exp( B_COST + draw[0] * SIGMA_SCALE ) ;

double ASC_C_RNP  = ASC_C + log(draw[1]) * SIGMA_C ;
double ASC_CS_RNP  = ASC_CS + draw[2] * SIGMA_CS ;
double ASC_CP_RNP  = ASC_CP + draw[3] * SIGMA_CP ;

utilities[0] =  1 * (ASC_W_RNP + B_COST_RND * B_TT_W_RND * data_tt_w_rp[i] / 60 );
//  U_2 =  1 * (ASC_W_RNP + B_COST_RND*B_TT_W_RND * data_tt_w_rp[i]/ 60 );

utilities[1] =  1 * (ASC_B_RNP + B_COST_RND * B_TT_B_RND * data_tt_b_rp[i] / 60 );"

e1 <- extract_variables(text1)

test_that("declarations are put into a template", {
  cpp_code <- as.character(convert_to_valid_cpp(test_template, e1))
  expect_equal(2, e1$num_utility_functions)
  expect_equal(strsplit(expected_output, "\n"), strsplit(cpp_code, "\n"))
})


test_that("draw replacement function works well", {
  expect_true(FALSE)
})



test_that("commented lines are handled correctly for all beta/utilities/draws replacement", {
  expect_true(FALSE)
})



test_that("a valid file processes without failing", {
  data_names <- c("data_tt_w_rp", "data_tt_b_rp")
  beta_names <- c("B_COST", "SIGMA_SCALE", "ASC_C", "SIGMA_C", "ASC_CS", "SIGMA_CS", "ASC_CP", "SIGMA_CP") ;
  
  expect_silent(validate_env(e1, data_names, beta_names))
})
