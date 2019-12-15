#' Runs a maximum likelihood estimation on a mixl choice model
#' 
#' This function performs a maximum likelihood estimation for choice models speficied using this package.
#' 
#' It is a wrapper for the maxLik function in the maxLik package. 
#' And additional arguments can be passed through to this function if required. 
#' 
#' @param model_spec The object that contains the loglikelihood function and other 
#' variables that help return better error messages. This function is best generated using the 
#' [specify_model()] function.
#' 
#' @param start_values A named vector of start values for the estimation. 
#' A warning and error will be given respectively if to many values are included or some are missing.
#' 
#' @param data A dataframe of the observations. It must include The columns CHOICE and ID, as well as 
#' columns for the variables specified in the utility function. The CHOICE variable must be from 1..k, 
#' where k is the number of  utility functions 
#' 
#' @param availabilities A 1/0 matrix of availabilities. The dimensions must be `nrows(data) * k`, where there are k utility functions. 
#' 
#' @param draws A numeric matrix of draws for calculating mixed effects. If there no mixed effects, this should be left null.
#' If the model specification included mixed effects, either this or `nDraws` need to be specified.
#' 
#' @param nDraws The number of draws to use in estimating a mixed model. 
#' Only needed if `draws` is left null. Then a matrix of normal halton draws will be generated.
#' 
#' @param fixedparam (optional) Coefficients which should be fixed to their starting values during estimation.
#' 
#' @param num_threads The maximum number of parallel cores to use in estimation. The default is 1. 
#' This should only be speficied on machines with an openMP compiler (linux and some OSXs).
#' 
#' @param ... futher arguments. such as control are passed to the maximisaiton routine in maxLik. 
#' See [maxLik::maxLik()] for more details
#' 
#' @return a mixl object that contains the results of the estimation 
#' 
#' @example R/examples/specify_model.R
#' 
#' @export 
estimate <- function (model_spec, start_values, data, availabilities,  
                           draws = NULL, nDraws = NULL, fixedparam = c(), num_threads=1, ...) {
  
  
  Nindividuals <- length(unique(data$ID))

  check_inputs(model_spec, start_values, data, availabilities, draws, fixedparam)
  
  draw_dimensions <- model_spec$draw_dimensions
  is_hybrid_choice <- model_spec$is_hybrid_choice
  is_mixed <- model_spec$is_mixed #TODO: change from is_mnl to a 'is_mixed' boolean
  
  if (is_mixed) { # we only want to pass the draws through if the loglik function is mixed
    if (missing(draws) & missing (nDraws)) {
      stop ("Either a draw matrix or the desired number of draws needs to be specified")  
    } else if (missing(draws) && !missing(nDraws)) {
      draws <- create_halton_draws(Nindividuals, nDraws, draw_dimensions)
      message(sprintf("Created a draw matrix of dimensions (%d, %d) for %d Individuals", nDraws, draw_dimensions, Nindividuals) )
    } else if (!is.matrix(draws)) {
      stop ("The draw parameter provieded is not a matrix")
    } else if (ncol(draws) < draw_dimensions | nrow(draws) < Nindividuals) {
      stop (sprintf("The draw matrix of dimensions %d x %d is not large enough (must be at least %d x %d)", nrow(draws), ncol(draws), Nindividuals, draw_dimensions))
    } 
    
    if (missing(nDraws)) nDraws <- as.integer(nrow(draws) / Nindividuals) #get the maxmimum possible number of draws available
    
    p <- matrix(0, nrow=Nindividuals, ncol=nDraws)
    
    ll2 <- function (betas) model_spec$logLik(betas, data, Nindividuals, availabilities, draws, nDraws, p, num_threads, p_indices=is_hybrid_choice)
    
  } else {
    p <- matrix(0, nrow=Nindividuals, ncol=1);
    
    ll2 <- function (betas) model_spec$logLik(betas, data, Nindividuals, availabilities, p, num_threads)
    
  }

  llsum <- function (betas) sum(ll2(betas))
  hessian_function <- function (betas) numDeriv::hessian(llsum, betas)
  
  mL <- maxLik::maxLik(ll2, start=start_values, fixed=fixedparam, method="BFGS",print.level=4, hess=hessian_function, ... )
  
  ### set up output
  mL$is_mixed     <- is_mixed
  mL$Nindividuals <- Nindividuals
  mL$nDraws       <- nDraws
  mL$choicetasks  <- nrow(data)
  mL$model_name   <- model_spec$model_name
  mL$data         <- data
  mL$availabilities <- availabilities
  mL$model_spec   <- model_spec
  
  est <- mL$estimate
  if (is_hybrid_choice) {
    mL$HybridLL <- ll2(est)
    
    ll2 <- function (betas) model_spec$logLik(betas, data, Nindividuals, availabilities, draws, nDraws, p, num_threads, p_indices=FALSE)
  }
  
  mL$zeroLL <- ll2(0*start_values)
  mL$initLL <- ll2(start_values)
  mL$finalLL <- ll2(est)
  mL$probabilities <- p
  mL$draws <- draws
  mL$rnd_equations <- model_spec$rnd_equations
  
  
  class(mL) <- c("mixl", class(mL))
  
  return (mL)
  
}

#' Check the inputs to the estimate function
#' 
#' This function checks the start_vlaues, data, availabilities, draws and fixedparams for validity.
#' If this function runs without error, then the inputs are valid for the maxLikelihood function. 
#' These checks are important, because an error in the internal C++ code will cause the Rstudio session to crash.
#' Incidentally, if there is concern of this happening, it is recommended to run the script from the 
#' command line, using Rscript.
#' 
#' @param model_spec The specified Model
#' @param start_values Named vector of proposed start values for the model
#' @param data the dataset on which to estimate
#' @param availabilities The availabilities for the alternatives in the model specification
#' @param draws The matrix of random draws
#' @param fixedparam Named vector of parameters to be fixed
#' @return Nothing
#' 
#' 
check_inputs <- function(model_spec, start_values, data, availabilities, draws, fixedparam) {
  #TODO: check existence of required data variables and CHOICE and ID
  
  #check betas
  start_value_names <- names(start_values)
  function_beta_names <- model_spec$beta_names
  
  beta_errors <- setdiff(function_beta_names, start_value_names)
  excess_betas <- setdiff(start_value_names, function_beta_names)
  
  Nindividuals <- length(unique(data$ID))
  k <- model_spec$num_utility_functions
  
  #check data is dataframe (again)
  if (!is.data.frame(data)) {
    stop("data argument must be a dataframe")
  }
  
  #check IDs are in range
  if (any(data$ID != as.integer(data$ID)) | min(data$ID) < 1 | max(data$ID) > Nindividuals) {
    stop(paste0("The individual IDs for this dataset must be integers in the range 1..", Nindividuals))
  }
  
  #check CHOICEs are in range
  if (any(data$CHOICE != as.integer(data$CHOICE)) | min(data$CHOICE) < 1 | max(data$CHOICE) > k) {
    stop(paste0("The Choices for this dataset must be integers in the range 1..", k))
  }
  
  #check availabilities are in range
  if (!is.matrix(availabilities) | nrow(availabilities) != nrow(data) | ncol(availabilities) != k) {
    stop(sprintf("The availabilities must be  matrix of the size %d x %d", nrow(data), k))
  }
  
  #check betas are all in the beta list
  if (length(beta_errors) > 0) {
    stop(paste("The following parameters are not named:", paste(beta_errors, collapse = ", ")))
  }
  
  if (length(excess_betas) > 0) {
    warning(paste("The following parameters are not used in the utility function but will be estimated anyway:", paste(excess_betas, collapse=",")))
  }
  
}
