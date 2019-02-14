context("Test detection of variables in source files")

text1 <- "//regexes:
//ignore lines starting with //

B_COST_RND = -exp( @B_COST + draw0 * @SIGMA_SCALE ) ;

ASC_C_RNP  = @ASC_C + log(draw9) * @SIGMA_C ;
ASC_CS_RNP  = @ASC_CS + draw10 * @SIGMA_CS ;
ASC_CP_RNP  = @ASC_CP + draw11 * @SIGMA_CP ;

U_1 =  1 * (ASC_W_RNP + B_COST_RND * B_TT_W_RND * $tt_w_rp / 60 );
//utilities[2] =  1 * (ASC_W_RNP + B_COST_RND*B_TT_W_RND * $tt_w_rp/ 60 );

U_2 =  1 * (ASC_B_RNP + B_COST_RND * B_TT_B_RND * $tt_b_rp / 60 );
"


text2 <- "//regexes:
B_COST_RND= -exp(@B_COST + draw0 * @SIGMA_SCALE) ;

ASC_C_RNP  =@ASC_C + log(draw9) * @SIGMA_C ;
ASC_CS_RNP  = @ASC_CS + draw10*@SIGMA_CS ;

U_1 =  1 * (ASC_W_RNP + B_COST_RND*B_TT_W_RND * $tt_w_rp/ 60 );

U_bus =  1 * (ASC_B_RNP + B_COST_RNDB_TT_B_RND *$tt_b_rp / 60);
"
e1 <- extract_variables(text1)
e2 <- extract_variables(text2)



test_that("mathematical operators not detected", {
  expect_identical(intersect(c("log", "exp", "draw"), e1$betas), character(0))
})

test_that("All data_cols are detected", {
  expect_equivalent(e1$data_cols, c("tt_w_rp", "tt_b_rp"))
})

test_that("All betas are detected", {
  expect_equivalent(e1$betas, c("B_COST", "SIGMA_SCALE", "ASC_C", "SIGMA_C", "ASC_CS",  "SIGMA_CS","ASC_CP", "SIGMA_CP"))
})

test_that("spacing after the variables is handled correctly", {
  expect_equivalent(e2$betas, c("B_COST", "SIGMA_SCALE", "ASC_C", "SIGMA_C", "ASC_CS",  "SIGMA_CS"))
  expect_equivalent(e2$data_cols, c("tt_w_rp", "tt_b_rp"))
  expect_equivalent(e2$new_vars, c("B_COST_RND", "ASC_C_RNP", "ASC_CS_RNP"))


})

test_that("utilities length - ignore commented lines", {
  expect_equal(length(e1$utility_function_names), 2)
  expect_equal(e1$num_utility_functions, 2)
})



test_that("handle datacols names that are subsets of each other: i.e. tt_w_rp_m23 and tt_w_rp", {
  
  text_subset <- "
B_COST_RND = -exp( @B_COST + draw_0 * @SIGMA_SCALE ) ;
ASC_W_RNP  = -exp( @B_COST + draw1 * @SIGMA_SCALE ) ;
B_TT_W_RND = -exp( @B_COST + draw2 * @SIGMA_SCALE ) ;

U_1 =  1 * (ASC_W_RNP + B_COST_RND * B_TT_W_RND * $tt_w_rp / 60 );
U_2 =  1 * (ASC_B_RNP + B_COST_RND * B_TT_W_RND * $tt_w_rp_m23 / 60 );
"
  
  test_template <-  "
!=== beta_declarations ===!

!=== data_declarations ===!

!=== draw_and_utility_declarations ===!
"
  
  cpp_expected <- "double B_COST = betas[\"B_COST\"];
double SIGMA_SCALE = betas[\"SIGMA_SCALE\"];

const NumericVector data_tt_w_rp = v.data[\"tt_w_rp\"];
const NumericVector data_tt_w_rp_m23 = v.data[\"tt_w_rp_m23\"];


double B_COST_RND = -exp( B_COST + draw[0] * SIGMA_SCALE ) ;
double ASC_W_RNP  = -exp( B_COST + draw[1] * SIGMA_SCALE ) ;
double B_TT_W_RND = -exp( B_COST + draw[2] * SIGMA_SCALE ) ;

utilities[0] =  1 * (ASC_W_RNP + B_COST_RND * B_TT_W_RND * data_tt_w_rp[i] / 60 );
utilities[1] =  1 * (ASC_B_RNP + B_COST_RND * B_TT_W_RND * data_tt_w_rp_m23[i] / 60 );"
                        
e1 <- extract_variables(text_subset)

expect_equivalent(e1$data_cols, c("tt_w_rp","tt_w_rp_m23"))

cpp_code <- as.character(convert_to_valid_cpp(test_template, e1))
expect_equal(e1$num_utility_functions, 2)
expect_equal(strsplit(cpp_code, "\n")[[1]], strsplit(cpp_expected, "\n")[[1]])


})


test_that("utilities length - ignore commented lines", {

text3 <- "//regexes:
B_COST_RND= -exp(@B_COST + draw0 * @SIGMA_SCALE) ;

ASC_C_RNP  =@ASC_C + log(draw9) * @SIGMA_C ;
ASC_CS_RNP  = @ASC_CS + draw10*@SIGMA_CS ;
ASC_U_1  = @ASC_CS + draw10*@SIGMA_CS ;
EU_1  = @ASC_CS + draw10*@SIGMA_CS ;

U_1 =  1 * (ASC_W_RNP + B_COST_RND*B_TT_W_RND * * EU_1 $tt_w_rp/ 60 );

U_bus =  1 * (ASC_B_RNP + B_COST_RNDB_TT_B_RND *$tt_b_rp / 60);
"
e3 <- extract_variables(text3)

expect_equal(length(e1$utility_function_names), 2)
expect_equal(e1$num_utility_functions, 2)
})
