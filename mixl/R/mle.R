

#' @export
maximumLikelihood <- function (logLik_function_env,
                               start_values, 
                               data, 
                               availabilities = NULL,  
                               draws = NULL,
                               fixedparam = c(), 
                               num_threads=1) {
  
    
    Nindividuals = length(unique(data$ID))
    draw_dimensions <- logLik_function_env$draw_dimensions
    is_hybrid_choice <- logLik_function_env$is_hybrid_choice
    
    draws <- create_draws(draws, Nindividuals, draw_dimensions)
    Ndraws = nrow(draws) / Nindividuals
    
    ######## missing availabilities handled in cpp code
    
    p <- matrix(0, nrow=Nindividuals, ncol=Ndraws);
    
    ll2 <- function (betas) logLik_function_env$logLik(betas, data, Nindividuals, availabilities, draws, Ndraws, p, num_threads, p_indices=is_hybrid_choice)
    llsum <- function (betas) sum(ll2(betas))
    hessian_function <- function (betas) numDeriv::hessian(llsum, beta1)
    
    maxLik_result <- maxLik::maxLik(ll2, start=start_values, fixed=fixedparam, method="BFGS",print.level=4, hess=hessian_function )
    
    model_output(maxLik_result)
       
}

#' @export
calculate_choice_likelihood <- function(logLik_function_env, 
                               betas, 
                               data, availabilities = NULL,  
                               draws = NULL, 
                               num_threads=1) {
  
  Nindividuals = length(unique(data$ID))
  draw_dimensions <- logLik_function_env$draw_dimensions
  
  draws <- create_draws(draws, Nindividuals, draw_dimensions)
  Ndraws = nrow(draws) / Nindividuals
  
  p <- matrix(0, nrow=Nindividuals, ncol=Ndraws)
  
  logLik_function_env$logLik(betas, data, Nindividuals, availabilities, draws, Ndraws, p, num_threads, p_indices=FALSE)
}

create_draws <- function(draws, Nindividuals, draw_dimensions) {
  if (class(draws) == "integer") {
    Ndraws = draws
    draws <- create_halton_draws(Nindividuals, Ndraws, draw_dimensions)
    message(sprintf("Created a draw matrix of dimensions (%d, %d) for %d Individuals", Ndraws, draw_dimensions, Nindividuals) )
  }
  else if (draw_dimensions == 0) {
    ##Then we have a normal MNL model
    stop("Standard MNL not yet implemented") #TODO describe this better?
    
  }
  if (class(draws) != "matrix") {
    stop("Either a draw matrix needs to be provided, or a desired number of draws provided") #TODO describe this better?
  }
  
  return (draws)
}
  