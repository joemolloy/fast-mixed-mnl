

#' @export
maximumLikelihood <- function (logLik_function_env,
                               start_values, 
                               data, 
                               availabilities = NULL, 
                               draws = NULL,
                               Ndraws = 5,
                               fixedparam = c(), 
                               num_threads=1) {
  
  
  # #check betas
  # e1$beta_errors <- setdiff(e1$betas, beta_names)
  # e1$excess_betas <- setdiff(beta_names, e1$betas)
  
  
  # #check betas are all in the beta list
  # if (length(e1$beta_errors) > 0) {
  #   valid = FALSE
  #   e1$error_messages <- c(e1$error_messages, paste("The following parameters are not named:", paste(e1$beta_errors, collapse = ", ")))
  # }
  # 
  # 
  # if (length(e1$excess_betas) > 0) {
  #   for (b in e1$excess_betas) {
  #     warning(paste("The following parameter was not used in the utility function but will be estimated anyway:", b))
  #   }
  # }
  #   
  
    Nindividuals = length(unique(data$ID))
    draw_dimensions <- logLik_function_env$draw_dimensions
    is_hybrid_choice <- logLik_function_env$is_hybrid_choice
    is_mixed <- logLik_function_env$is_mixed #TODO: change from is_mnl to a 'is_mixed' boolean
    
    if (is_mixed) {
      if (missing(draws)) {
        create_halton_draws(Nindividuals, Ndraws, draw_dimensions)
        message(sprintf("Created a draw matrix of dimensions (%d, %d) for %d Individuals", Ndraws, draw_dimensions, Nindividuals) )
      } else if (ncol(draws) != draw_dimensions && nrow(draws)) {
        stop (sprintf("the draw matrix of size (%d, %d) does not match that required"))
      }
  #    Ndraws = floor(nrow(draws) / Nindividuals) #get the maxmimum possible number of draws available
      
      p <- matrix(0, nrow=Nindividuals, ncol=Ndraws)

      ll2 <- function (betas) logLik_function_env$logLik(betas, data, Nindividuals, availabilities, draws, Ndraws, p, num_threads, p_indices=is_hybrid_choice)
      
    } else {
      p <- matrix(0, nrow=Nindividuals, ncol=1);
      
      ll2 <- function (betas) logLik_function_env$logLik(betas, data, Nindividuals, availabilities, p, num_threads)
      
    }
    ######## missing availabilities handled in cpp code
    
    llsum <- function (betas) sum(ll2(betas))
    hessian_function <- function (betas) numDeriv::hessian(llsum, betas)
    
    mL <- maxLik::maxLik(ll2, start=start_values, fixed=fixedparam, method="BFGS",print.level=4, hess=hessian_function )
    
    ### set up output
    
    if (mL$code == 0) {
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
