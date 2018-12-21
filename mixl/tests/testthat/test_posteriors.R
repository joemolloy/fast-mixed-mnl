context("Test posterior function")

test_that("Random equations are parsed", {
  
  mnl_test <- "
      ASC_A_RND 	= @ASC_A 	+ draw_1 * @SIGMA_A1 		+ draw_7 * @SIGMA_A2;
  ASC_B_RND 	= @ASC_B 	+ draw_2 * @SIGMA_B;
  ASC_B 	= @ASC_B 	+ draw_2 * @SIGMA_B;
  
  U_A = ASC_A_RND + @B_price * $price_A / 1000 + @B_time * $time_A / 60 + @B_change * $change_A; 
  U_B = ASC_B_RND + @B_price * $price_B / 1000 + @B_timeB * $time_B / 60;
  "
  
  eqs <- mixl::parse_equations(mnl_test)
  expect_equal(colnames(eqs), c("name", "equation"))
  expect_equal(dim(eqs), c(2,2))
})

test_that("Simple posterior calcualtion", {
  
  library(Rcpp)
  library(Matrix)
  data("Train", package="mlogit")
  Train$ID <- Train$id
  Train$CHOICE <- as.numeric(Train$choice)
  
  head(Train, 3)
  
  mnl_test <- "
      ASC_A_RND 	= @ASC_A 	+ draw_1 * @SIGMA_A1 		+ draw_7 * @SIGMA_A2;
      ASC_B_RND 	= @ASC_B 	+ draw_2 * @SIGMA_B;
  
      U_A = ASC_A_RND + @B_price * $price_A / 1000 + @B_time * $time_A / 60 + @B_change * $change_A; 
      U_B = ASC_B_RND + @B_price * $price_B / 1000 + @B_timeB * $time_B / 60;
    "
  
  template_location <- system.file("include", "mixl", "cpp_posteriors.cpp", package = "mixl")
  #template_location <- "inst/include/mixl/cpp_posteriors.cpp"
  
  #only take starting values that are needed
  est <- setNames(c(-1059.69729,  -181.27796,  -251.78909,  -241.18878,   -86.77386,  -173.09451,   291.02618,   142.71793,   332.60909)
                  , c("B_price", "B_time", "B_timeB", "B_change", "ASC_A", "ASC_B", "SIGMA_A1", "SIGMA_A2", "SIGMA_B"))
  
  availabilities <- mixl::generate_default_availabilities(Train, 2)
  
  logLik_env <- mixl::specify_model(mnl_test, Train, compile=TRUE)
  model <- mixl::estimate(logLik_env, est, Train, availabilities = availabilities, nDraws = 20)
  
  cpp_post <- mixl::posteriors(model)
  
  print("cpp done")
  
  draws <- model$draws
  p <- model$probabilities
  
  #calculate in R
  asc_a_rnd <- est["ASC_A"] + draws[,1] * est["SIGMA_A1"] + draws[,2] * est["SIGMA_A2"]
  asc_a_rnd <- matrix(asc_a_rnd, ncol = 20, byrow = T)
  asc_a_rnd_means <- rowMeans(p * asc_a_rnd) / rowMeans(p)

  asc_b_rnd <- est["ASC_B"] 	+ draws[,3] * est["SIGMA_B"];
  asc_b_rnd <- matrix(asc_b_rnd, ncol = 20, byrow = T)
  asc_b_rnd_means <- rowMeans(p * asc_b_rnd) / rowMeans(p)
  
  r_posteriors <- cbind("ASC_A_RND" = asc_a_rnd_means, "ASC_B_RND" = asc_b_rnd_means)

  expect_equal(cpp_post, r_posteriors, tolerance=1e-4)
  #cbind(cpp_post[,1], r_posteriors[,1])
})

# 
# NumericMatrix random_coeffs(v.draws.nrows(), num_rnd_var);
# for (int i = 0; i<v.draws.nrows(); i++) {
#   int r_idx = 0;
#   random_coeffs(i, r_idx++) = 0.0;
# }

