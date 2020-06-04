library(testthat)

context("Test prediction")

data("Train", package="mlogit")
Train$ID <- Train$id
Train$CHOICE <- as.integer(Train$choice)
Nindividuals <- length(unique(Train$ID))

skip_on_cran()
test_that("The prediction code compiles and runs", {
  mnl_test <- "
    ASC_B_RND 	= @ASC_B 	+ draw_2 * @SIGMA_B;
  
  U_A =             @B_price * $price_A / 1000 + @B_time * $time_A / 60 + @B_change * $change_A; 
  U_B = ASC_B_RND + @B_price * $price_B / 1000 + @B_timeB * $time_B / 60 ;
  "
#  Sys.setenv("PKG_CPPFLAGS"= sprintf("-I\"%s\"", system.file(package = "mixl", "include")))
  
  logLik_env <- mixl::specify_model(mnl_test, Train, disable_multicore=T)
  
  #only take starting values that are needed
  est <- stats::setNames(c(-1.03970347, -0.80712567, -0.95341969, -0.14061543,  0.19796530, -0.01888506), 
                  c("B_price", "B_time", "B_timeB", "B_change", "ASC_B","SIGMA_B"))
  
  availabilities <- mixl::generate_default_availabilities(Train, logLik_env$num_utility_functions)
  nDraws <- 5
  draws <- mixl::create_halton_draws(Nindividuals, 5, 1)
  
  model <- mixl::estimate(logLik_env, est, Train, availabilities, draws)
  
  model_preds <- mixl::probabilities(model, num_threads=1)
  
  expect_equal(dim(model_preds), c(nrow(Train), 4 + logLik_env$num_utility_functions))
  expect_equal(colnames(model_preds), c( "i", "ID", "choice_index", "p_choice", "p_1", "p_2"))

  expect_equal(unname(model_preds[5,]), c(4, 0 ,2 ,0.2722314, 0.727768623, 0.2722314), tolerance=1e-6)

})


skip_on_cran()
test_that("mnl prediction", {
  
  
  mnl_test <- "
PRICE_RND 	= -exp(@B_price * $price_A / 1000) * pow($price_A /1000, @LAMDBA_DIST_COST) ;
ASC_A_RND 	= @ASC_A;

U_A = ASC_A_RND + PRICE_RND + @B_time * $time_A / 60 + @B_change * $change_A; 
U_B = @B_price * $price_B / 1000 + @B_timeB * $time_B / 60;
"
  
  #only take starting values that are needed
  est <- stats::setNames(c(-0.1729610, -0.2057692, -0.1250778, -0.0649737, -0.1804503, 0.1)
                  , c("B_price", "B_time", "B_timeB", "B_change", "ASC_A", "LAMDBA_DIST_COST"))
  
  availabilities <- mixl::generate_default_availabilities(Train, 2)
  
  logLik_env <- mixl::specify_model(mnl_test, Train)
  
  model <- mixl::estimate(logLik_env, est, Train, availabilities = availabilities)
  model_preds <- mixl::probabilities(model, num_threads=1)
  
  expect_equal(dim(model_preds), c(nrow(Train), 4 + logLik_env$num_utility_functions))
  expect_equal(colnames(model_preds), c( "i", "ID", "choice_index", "p_choice", "p_1", "p_2"))
  
  expect_equal(unname(model_preds[5,]), c(4, 0 ,2 ,0.37571567, 0.6242843, 0.37571567), tolerance=1e-6)
  
})


skip_on_cran()
test_that("Different data can be used in mixed mnl", {
  
  Train2 <- data.frame(Train)
  Train2$time_A <- Train2$time_A * 1.1
  
  
  mnl_test <- "
  ASC_B_RND 	= @ASC_B 	+ draw_2 * @SIGMA_B;
  
  U_A =             @B_price * $price_A / 1000 + @B_time * $time_A / 60 + @B_change * $change_A; 
  U_B = ASC_B_RND + @B_price * $price_B / 1000 + @B_timeB * $time_B / 60 ;
  "
  #  Sys.setenv("PKG_CPPFLAGS"= sprintf("-I\"%s\"", system.file(package = "mixl", "include")))
  
  logLik_env <- mixl::specify_model(mnl_test, Train, disable_multicore=T)
  
  #only take starting values that are needed
  est <- stats::setNames(c(-1.03970347, -0.80712567, -0.95341969, -0.14061543,  0.19796530, -0.01888506), 
                  c("B_price", "B_time", "B_timeB", "B_change", "ASC_B","SIGMA_B"))
  
  availabilities <- mixl::generate_default_availabilities(Train, logLik_env$num_utility_functions)
  nDraws <- 5
  draws <- mixl::create_halton_draws(Nindividuals, 5, 1)
  
  model <- mixl::estimate(logLik_env, est, Train, availabilities, draws)
  
  model_preds <- mixl::probabilities(model, data = Train2, num_threads=1)
  
  expect_equal(dim(model_preds), c(nrow(Train), 4 + logLik_env$num_utility_functions))
  expect_equal(colnames(model_preds), c( "i", "ID", "choice_index", "p_choice", "p_1", "p_2"))
  
  expect_equal(unname(model_preds[5,]), c(4, 0 ,2 , 0.3140528, 0.685947201, 0.3140528), tolerance=1e-6)
  
})