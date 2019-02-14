context("Test replacement and intepretation of variables in source files")

text1 <- "//regexes:
//ignore lines starting with //

B_COST_RND = -exp( @B_COST + draw_0 * @SIGMA_SCALE ) ;

ASC_C_RNP  = @ASC_C + log(draw_bus) * @SIGMA_C ;
ASC_CS_RNP  = @ASC_CS + draw_8 * @SIGMA_CS ;
ASC_CP_RNP  = @ASC_CP + draw_11 * @SIGMA_CP ;

U_1 =  1 * (ASC_W_RNP + B_COST_RND * B_TT_W_RND * $tt_w_rp / 60 );
//  U_2 =  1 * (ASC_W_RNP + B_COST_RND*B_TT_W_RND * $tt_w_rp/ 60 );

U_3 =  1 * (ASC_B_RNP + B_COST_RND * B_TT_B_RND * $tt_b_rp / 60 );
"

test_template <-  "
!=== beta_declarations ===!

!=== data_declarations ===!

!=== draw_and_utility_declarations ===!
"


expected_output = "double B_COST = betas[\"B_COST\"];
double SIGMA_SCALE = betas[\"SIGMA_SCALE\"];
double ASC_C = betas[\"ASC_C\"];
double SIGMA_C = betas[\"SIGMA_C\"];
double ASC_CS = betas[\"ASC_CS\"];
double SIGMA_CS = betas[\"SIGMA_CS\"];
double ASC_CP = betas[\"ASC_CP\"];
double SIGMA_CP = betas[\"SIGMA_CP\"];

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



test_that("draw replacement function works well", {
  
  draws_exp <- c("draw_0", "draw_bus", "draw_8", "draw_11")
  draw_dimensions_exp <- 4
  
  expect_equal(e1$draw_dimensions, draw_dimensions_exp)
  expect_equal(e1$draws, draws_exp)
})


test_that("a valid file processes without failing", {
  data_names <- c("data_tt_w_rp", "data_tt_b_rp")
  beta_names <- c("B_COST", "SIGMA_SCALE", "ASC_C", "SIGMA_C", "ASC_CS", "SIGMA_CS", "ASC_CP", "SIGMA_CP") ;
  
  expect_silent(validate_env(e1, data_names))
})

test_that("declarations are put into a template", {
  cpp_code <- as.character(convert_to_valid_cpp(test_template, e1))
  expect_equal(e1$num_utility_functions, 2)
  expect_equal(strsplit(cpp_code, "\n")[[1]], strsplit(expected_output, "\n")[[1]])
})

