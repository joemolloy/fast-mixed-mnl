context("Test detection of variables in source files")

text1 <- "//regexes:
//ignore lines starting with //

B_COST_RND = -exp( @B_COST + draw0 * @SIGMA_SCALE ) ;

ASC_C_RNP  = @ASC_C + log(draw9) * @SIGMA_C ;
ASC_CS_RNP  = @ASC_CS + draw10 * @SIGMA_CS ;
ASC_CP_RNP  = @ASC_CP + draw11 * @SIGMA_CP ;

utilities[0] =  1 * (ASC_W_RNP + B_COST_RND * B_TT_W_RND * $tt_w_rp / 60 );
//utilities[2] =  1 * (ASC_W_RNP + B_COST_RND*B_TT_W_RND * $tt_w_rp/ 60 );

utilities[1] =  1 * (ASC_B_RNP + B_COST_RND * B_TT_B_RND * $tt_b_rp / 60 );
"


text2 <- "//regexes:
B_COST_RND= -exp(@B_COST + draw0 * @SIGMA_SCALE) ;

ASC_C_RNP  =@ASC_C + log(draw9) * @SIGMA_C ;
ASC_CS_RNP  = @ASC_CS + draw10*@SIGMA_CS ;

utilities[0] =  1 * (ASC_W_RNP + B_COST_RND*B_TT_W_RND * $tt_w_rp/ 60 );

utilities[1] =  1 * (ASC_B_RNP + B_COST_RNDB_TT_B_RND *$tt_b_rp / 60);
"
e1 <- extract_variables(text1)
e2 <- extract_variables(text2)



test_that("mathematical operators not detected", {
  expect_identical(character(0), intersect(c("log", "exp", "draw"), e1$betas))
})

test_that("All data_cols are detected", {
  expect_equivalent(c("tt_w_rp", "tt_b_rp"), e1$data_cols)
})

test_that("All betas are detected", {
  expect_equivalent(c("B_COST", "SIGMA_SCALE", "ASC_C", "SIGMA_C", "ASC_CS",  "SIGMA_CS","ASC_CP", "SIGMA_CP"), e1$betas)
})

test_that("spacing after the variables is handled correctly", {
  expect_equivalent(c("B_COST", "SIGMA_SCALE", "ASC_C", "SIGMA_C", "ASC_CS",  "SIGMA_CS"), e2$betas)
  expect_equivalent(c("tt_w_rp", "tt_b_rp"), e2$data_cols)
  expect_equivalent(c("B_COST_RND", "ASC_C_RNP", "ASC_CS_RNP"), e2$new_vars)


})

test_that("utilities length - ignore commented lines", {
  expect_equal(2, length(e1$util_lines))
})

bad_text1 = ""
