context("Test validation of variables in source files")

data_names <- c("tt_w_rp", "tt_b_rp")
beta_names <- c("B_COST", "SIGMA_SCALE", "ASC_C", "SIGMA_C", "ASC_CS",  "SIGMA_CS")






test_that("perfect input", {
  
  e1 <- new.env()
  e1$data_cols <- c("tt_w_rp", "tt_b_rp")
  e1$betas <- c("B_COST", "SIGMA_SCALE", "ASC_C", "SIGMA_C", "ASC_CS",  "SIGMA_CS")
  e1$new_vars <- c("B_COST_RND", "ASC_C_RNP", "ASC_CS_RNP")
  
  valid <- validate_env(e1, data_names)
  
  expect_true(valid)
  expect_equal(e1$error_messages, c())
  
})


test_that("data error", {
  
  e1 <- new.env()
  e1$data_cols <- c("tt_w_rp", "tt_b_rp", "tt_b_rp2")
  e1$betas <- c("B_COST", "SIGMA_SCALE", "ASC_C", "SIGMA_C", "ASC_CS",  "SIGMA_CS")
  e1$new_vars <- c("B_COST_RND", "ASC_C_RNP", "ASC_CS_RNP")
  
  valid <- validate_env(e1, data_names)
  
  expect_false(valid)
  expect_equal(e1$error_messages, c(paste("The following variables are not available in the dataset:", "tt_b_rp2" )))
  
})

test_that("multiple errors", {
  
  e1 <- new.env()
  e1$data_cols <- c("tt_w_rp", "tt_b_rp", "tt_b_rp2")
  e1$betas <- c("B_COST", "SIGMA_SCALE", "ASC_C", "ASC_CS",  "SIGMA_CS", "SIGMA_SCALE2")
  e1$new_vars <- c("B_COST_RND", "ASC_C_RNP", "ASC_C")

  
  expected_errors <- c("The following variables are not available in the dataset: tt_b_rp2",
                       "The following new variables have the same names as parameters: ASC_C")
  
  valid <- validate_env(e1, data_names)
  
  expect_false(valid)
  expect_length(e1$error_messages, 2)
  expect_equal(e1$error_messages, expected_errors)
  
})
