

#' @export
maxLikelihood <- function (logLik_function_env, start_values, data, availabilities, 
                           draws = NULL, Ndraws = NULL, fixedparam = c(), num_threads=1) {
  
  #check betas
  start_value_names <- names(start_values)
  function_beta_names <- logLik_function_env$beta_names
  beta_errors <- setdiff(function_beta_names, start_value_names)
  excess_betas <- setdiff(start_value_names, function_beta_names)
  
  Nindividuals <- length(unique(data$ID))
  k <- logLik_function_env$num_utility_functions
  
  #check IDs are in range
  if (!is.integer(data$ID) | min(data$ID) < 1 | max(data$ID) > Nindividuals) {
    stop(paste("The individual IDs for this dataset must be integers in the range 1..", Nindividuals))
  }
  
  #check CHOICEs are in range
  if (!is.integer(data$CHOICE) | min(data$CHOICE) < 1 | max(data$CHOICE) > k) {
    stop(paste("The Choices for this dataset must be integers in the range 1..", k))
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
  
  
  
  draw_dimensions <- logLik_function_env$draw_dimensions
  is_hybrid_choice <- logLik_function_env$is_hybrid_choice
  is_mixed <- logLik_function_env$is_mixed #TODO: change from is_mnl to a 'is_mixed' boolean
  
  if (is_mixed) { # we only want to pass the draws through if the loglik function is mixed
    if (missing(draws) & missing (Ndraws)) {
      stop ("Either a draw matrix or the desired number of draws needs to be specified")  
    } else if (missing(draws) && !missing(Ndraws)) {
      draws <- create_halton_draws(Nindividuals, Ndraws, draw_dimensions)
      message(sprintf("Created a draw matrix of dimensions (%d, %d) for %d Individuals", Ndraws, draw_dimensions, Nindividuals) )
    } else if (!is.matrix(draws)) {
      stop ("The draw parameter provieded is not a matrix")
    } else if (ncol(draws) < draw_dimensions | nrow(draws) < Nindividuals) {
      stop (sprintf("The draw matrix of dimensions %d x %d is not large enough (must be at least %d x %d)", nrow(draws), ncol(draws), Nindividuals, draw_dimensions))
    } 
    
    Ndraws = as.integer(nrow(draws) / Nindividuals) #get the maxmimum possible number of draws available
    
    p <- matrix(0, nrow=Nindividuals, ncol=Ndraws)
    
    ll2 <- function (betas) logLik_function_env$logLik(betas, data, Nindividuals, availabilities, draws, Ndraws, p, num_threads, p_indices=is_hybrid_choice)
    
  } else {
    p <- matrix(0, nrow=Nindividuals, ncol=1);
    
    ll2 <- function (betas) logLik_function_env$logLik(betas, data, Nindividuals, availabilities, p, num_threads)
    
  }

  llsum <- function (betas) sum(ll2(betas))
  hessian_function <- function (betas) numDeriv::hessian(llsum, betas)
  
  mL <- maxLik::maxLik(ll2, start=start_values, fixed=fixedparam, method="BFGS",print.level=4, hess=hessian_function )
  
  ### set up output
  
  if (mL$code == 0) { #successful convergence, calculate all the metrics
    est <- mL$estimate
    if (is_hybrid_choice) {
      mL$HybridLL <- ll2(est)
      
      ll2 <- function (betas) logLik_function_env$logLik(betas, data, Nindividuals, availabilities, draws, Ndraws, p, num_threads, p_indices=FALSE)
    }
    
    mL$zeroLL <- sum(ll2(0*start_values))
    mL$initLL <- sum(ll2(start_values))
    mL$finalLL <- sum(ll2(est))
    
    mL$Nindividuals <- Nindividuals
    mL$Ndraws       <- Ndraws
    
    class(mL) <- c("mixl", class(mL))
    
  }
  
  
  maxLik_result <- mL
  
  return (maxLik_result)
  
}
