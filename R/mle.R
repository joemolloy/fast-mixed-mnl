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
#' @param weights (optional) A vector of weights (vector length must equal the number of observations).
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
                           draws, nDraws, fixedparam = c(), num_threads=1, weights = NULL, ...) {
  
  start_time = Sys.time()
  
  check_inputs(model_spec, start_values, data, availabilities, draws, fixedparam, weights)
  
  Nindividuals <- length(unique(data$ID))
  
  if (missing(weights) || is.null(weights)) {
    weights <- rep(1, nrow(data))
  }
  

  draw_dimensions <- model_spec$draw_dimensions
  is_hybrid_choice <- model_spec$is_hybrid_choice
  is_mixed <- model_spec$is_mixed 

  
  if (is_mixed) { # we only want to pass the draws through if the loglik function is mixed
    list2env(check_draw_inputs(draws, nDraws, draw_dimensions, Nindividuals), envir = environment())
  } else {
    nDraws = 1
    draws = NULL
  }
  
  #check for hybrid choice that individual level weights are used
  if (is_hybrid_choice) {
    num_weights = unlist(lapply(split(data, data$ID), function(x) { length(unique(weights[as.integer(rownames(x))])) } ))
    if (any(num_weights > 1)) {
      warning(paste(
        'Observation level weights detected, but a hybrid choice model is being estimated.',
        'The specification assumed that there is only one unique weight per individual.'))
    }

  }
  
  p <- matrix(0, nrow=Nindividuals, ncol=nDraws)
    
  ll2 <- function (betas) model_spec$logLik(betas, data, Nindividuals, availabilities, draws, nDraws, p, weights, num_threads, p_indices=is_hybrid_choice)
    
  llsum <- function (betas) sum(ll2(betas))
  hessian_function <- function (betas) numDeriv::hessian(llsum, betas)
  
  mL <- maxLik::maxLik(ll2, start=start_values, fixed=fixedparam, method="BFGS", print.level=4, hess=hessian_function, ... )
  
  runtime = Sys.time() - start_time
  
  ### set up output
  mL$runtime     <- runtime
  mL$is_mixed     <- is_mixed
  mL$Nindividuals <- Nindividuals
  mL$choicetasks  <- nrow(data)
  mL$model_name   <- model_spec$model_name
  mL$data         <- data
  mL$availabilities <- availabilities
  mL$model_spec   <- model_spec
  mL$weights <- weights
  
  est <- mL$estimate
  
  mL$probabilities <- p
  mL$rnd_equations <- model_spec$rnd_equations
  
  if (is_mixed) {
    mL$nDraws <- nDraws
    mL$draws <- draws
  }
  
  mL$zeroLL <- ll2(0*start_values)
  mL$initLL <- ll2(start_values)
  mL$finalLL <- ll2(est)
  mL$choiceLL <- ll2(est)
  
  if (is_hybrid_choice) {
    ll2 <- function (betas) model_spec$logLik(betas, data, Nindividuals, availabilities, draws, nDraws, p, weights, num_threads, p_indices=FALSE)
    mL$choiceLL <- ll2(est)
  }
  
  class(mL) <- c("mixl", class(mL))
  
  return (mL)
  
}
