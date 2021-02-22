context("Test estimate function")

data("Train", package="mlogit")
Train$ID <- Train$id
Train$CHOICE <- as.numeric(Train$choice)


test_that("A basic MNL model converges and creates the output", {

    mnl_test <- "
    U_A = @B_price * $price_A / 1000 + @B_time * $time_A / 60 + @B_change * $change_A;
    U_B = @B_price * $price_B / 1000 + @B_timeB * $time_B / 60 ;
    "

    logLik_env <- mixl::specify_model(mnl_test, Train, rebuild=T)

    #only take starting values that are needed
    est <- stats::setNames(c(1,1,1,1), c("B_price", "B_time", "B_timeB", "B_change"))

    availabilities <- mixl::generate_default_availabilities(Train, logLik_env$num_utility_functions)

    model <- mixl::estimate(logLik_env, est, Train, availabilities = availabilities, num_threads = 1, control=list(iterlim=4))

    expect_equal(model$code, 1) #iteration limit
    expect_equal(model$maximum, -2046.955200, tolerance=1e-3)
    expect_s3_class(model, "mixl")
    expect_s3_class(summary.mixl(model), "summary.mixl")
    prints_text(summary(model)) #TODO::::: nDraws not found when printing mnl model
})


skip_on_cran()
test_that("A mixed MNL model converges and creates the output", {
  #randomly assign observations to ID's
  
  data("Train", package="mlogit")
  Train$ID <- Train$id
  Train$CHOICE <- as.numeric(Train$choice)
  
  mnl_test <- "
    ASC_B_RND 	= @ASC_B 	+ draw_2 * @SIGMA_B;

    U_A =             @B_price * $price_A / 1000 + @B_time * $time_A / 60 + @B_change * $change_A; 
    U_B = ASC_B_RND + @B_price * $price_B / 1000 + @B_timeB * $time_B / 60;
  "
  
  model_spec <- mixl::specify_model(mnl_test, Train)
  
  #only take starting values that are needed
  est <- stats::setNames(c(0,0,0,0,0,0), c("B_price", "B_time", "B_timeB", "B_change", "ASC_B","SIGMA_B"))
  
  availabilities <- mixl::generate_default_availabilities(Train, model_spec$num_utility_functions)
  
  #weights <- runif(nrow(Train))+1
  #weights <- weights * (nrow(Train) / sum(weights))
  
  system.time(model <- mixl::estimate(model_spec, est, Train, availabilities = availabilities, nDraws = 100, num_threads=1))
  
  expect_equal(model$code, 0)
  expect_length(model$estimate, 6)
  expect_equal(model$maximum, -1842.243, tolerance=1e-3)
  
  expect_s3_class(model, "mixl")
  expect_s3_class(summary.mixl(model), "summary.mixl")
  prints_text(summary(model))
})

skip_on_cran()
test_that("A mixed MNL model failes : not enough betas", {
  #randomly assign observations to ID's
  mnl_test <- "
    ASC_A_RND 	= @ASC_A 	+ draw_1 * @SIGMA_A1 		+ draw_7 * @SIGMA_A2;
    ASC_B_RND 	= @ASC_B 	+ draw_2 * @SIGMA_B;

    U_A = ASC_A_RND + @B_price * $price_A / 1000 + @B_time * $time_A / 60 + @B_change * $change_A; 
    U_B = ASC_B_RND + @B_price * $price_B / 1000 + @B_timeB * $time_B / 60;
  "
  
  logLik_env <- mixl::specify_model(mnl_test, Train, compile=TRUE, disable_multicore=T)
  
  #only take starting values that are needed
  est <- stats::setNames(c(1,1,1,1), c("B_price", "B_time", "B_timeB", "B_change"))
  
  availabilities <- mixl::generate_default_availabilities(Train, logLik_env$num_utility_functions)
  exp_error <- "The following parameters are not named: ASC_A, SIGMA_A1, SIGMA_A2, ASC_B, SIGMA_B"
  expect_error(model <- mixl::estimate(logLik_env, est, Train, availabilities = availabilities, nDraws = 5), exp_error)
  
  est <- stats::setNames(c(1,1,1,1, 0.1, 0.1, 0.1, 0.1, 0.1, 0), c("B_price", "B_time", "B_timeB", "B_change", "ASC_A", "ASC_B", "SIGMA_A1", "SIGMA_A2", "SIGMA_B", "SIG_EXTRA"))
  
  exp_warning <- "The following parameters are not used in the utility function but will be estimated anyway: SIG_EXTRA"
  expect_warning(model <- mixl::estimate(logLik_env, est, Train, availabilities = availabilities, nDraws = 5), exp_warning)
  
  
})



skip_on_cran()
test_that("creating and validating draws", {
  
  
  #randomly assign observations to ID's
  mnl_test <- "
    ASC_A_RND 	= @ASC_A 	+ draw_1 * @SIGMA_A1 		+ draw_7 * @SIGMA_A2;
    ASC_B_RND 	= @ASC_B 	+ draw_2 * @SIGMA_B;

    U_A = ASC_A_RND + @B_price * $price_A / 1000 + @B_time * $time_A / 60 + @B_change * $change_A; 
    U_B = ASC_B_RND + @B_price * $price_B / 1000 + @B_timeB * $time_B / 60;
  "
  
  
  
  logLik_env <- mixl::specify_model(mnl_test, Train, compile=TRUE, disable_multicore=T)
  
  #only take starting values that are needed
  est <- stats::setNames(c(1,1,1,1, 0.1, 0.1, 0.1, 0.1, 0.1), c("B_price", "B_time", "B_timeB", "B_change", "ASC_A", "ASC_B", "SIGMA_A1", "SIGMA_A2", "SIGMA_B"))
  
  availabilities <- mixl::generate_default_availabilities(Train, logLik_env$num_utility_functions)

  exp_error <- "Either a draw matrix or the desired number of draws needs to be specified"
  expect_error(model <- mixl::estimate(logLik_env, est, Train, availabilities = availabilities), exp_error)
  
  draws <- mixl::create_halton_draws(100, 1, logLik_env$draw_dimensions)
  exp_error <- "The draw matrix of dimensions 100 x 3 is not large enough (must be at least 235 x 3)"
  expect_error(model <- mixl::estimate(logLik_env, est, Train, availabilities = availabilities, draws = draws), exp_error, fixed=TRUE)
  
  Nindividuals <- length(unique(Train$ID))
  
  draws <- mixl::create_halton_draws(Nindividuals, 1, 1)
  exp_error <- "The draw matrix of dimensions 235 x 1 is not large enough (must be at least 235 x 3)"
  expect_error(model <- mixl::estimate(logLik_env, est, Train, availabilities = availabilities, draws = draws), exp_error, fixed=TRUE)
  
  
  exp_message <- "Created a draw matrix of dimensions (5, 3) for 235 Individuals"
  expect_message(model <- mixl::estimate(logLik_env, est, Train, availabilities = availabilities, nDraws = 5), exp_message, fixed=TRUE)
  
  
  
})

skip_on_cran()
test_that("hybrid choice", {
  
})

