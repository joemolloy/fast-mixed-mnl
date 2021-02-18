library(mixl)

data("Train", package="mlogit")
Train$ID <- Train$id
Train$CHOICE <- as.numeric(Train$choice)


skip_on_cran()
test_that("Simple utilities calcualtion", {
    
  est <- stats::setNames(c(1,1,1,1), c("B_price", "B_time", "B_timeB", "B_change"))
  
  availabilities <- mixl::generate_default_availabilities(Train, 2)
  
  Nindividuals <- length(unique(Train$ID))
  
  utility_script <- "
      U_A = @B_price * $price_A / 1000 + @B_time * $time_A / 60 + @B_change * $change_A;
      U_B = @B_price * $price_B / 1000 + @B_timeB * $time_B / 60 ;
      "
  
  model_spec <- specify_model(utility_script, Train)
  
  utilities_matrix = mixl::utilities(model_spec, est, Train, availabilities, NULL)
  
  utilities_matrix
})


skip_on_cran()
test_that("Simple utilities calcualtion (Mixed)", {
  

  availabilities <- mixl::generate_default_availabilities(Train, 2)
  

  mnl_test <- "
    ASC_B_RND 	= @ASC_B 	+ draw_2 * @SIGMA_B;

    U_A =             @B_price * $price_A / 1000 + @B_time * $time_A / 60 + @B_change * $change_A; 
    U_B = ASC_B_RND + @B_price * $price_B / 1000 + @B_timeB * $time_B / 60;
  "
  
  model_spec <- mixl::specify_model(mnl_test, Train)
  
  #only take starting values that are needed
  est <- stats::setNames(c(0,0,0,0,0,0), c("B_price", "B_time", "B_timeB", "B_change", "ASC_B","SIGMA_B"))
  
  model <- mixl::estimate(model_spec, est, Train, availabilities, nDraws = 10)
  
  utilities_matrix = mixl::utilities(model_spec, model$estimate, Train, availabilities, model$draws)
  
  utilities_matrix
})