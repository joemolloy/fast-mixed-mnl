
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
#' @param weights The weights vector
#' @return Nothing
#' 
#' 
check_inputs <- function(model_spec, start_values, data, availabilities, draws, fixedparam, weights) {
  
  #check existence of required data variables and CHOICE and ID
  if (length(intersect(c('ID', 'CHOICE'), colnames(data))) != 2) {
    stop("Data argument must contain ID and CHOICE columns, see the documentation for more details")
  }
  
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
  if (any(data$ID != as.integer(data$ID)) || min(data$ID) < 1 || max(data$ID) > Nindividuals) {
    stop(paste0("The individual IDs for this dataset must be integers in the range 1..", Nindividuals))
  }
  
  #check CHOICEs are in range
  if (any(data$CHOICE != as.integer(data$CHOICE)) || min(data$CHOICE) < 1 || max(data$CHOICE) > k) {
    stop(paste0("The Choices for this dataset must be integers in the range 1..", k))
  }
  
  #check availabilities are in range
  if (!is.matrix(availabilities) || nrow(availabilities) != nrow(data) || ncol(availabilities) != k) {
    stop(sprintf("The availabilities must be  matrix of the size %d x %d", nrow(data), k))
  }
  
  #check betas are all in the beta list
  if (length(beta_errors) > 0) {
    stop(paste("The following parameters are not named:", paste(beta_errors, collapse = ", ")))
  }
  
  if (length(excess_betas) > 0) {
    warning(paste("The following parameters are not used in the utility function but will be estimated anyway:", paste(excess_betas, collapse=",")))
  }
  
  if (!missing(weights) && !is.null(weights) && !all.equal(length(weights), nrow(data))) {
    warning(sprintf("The length of the weights vector is not the same as the number of observations (%d vs %d(", length(weights), nrow(data)))
  }
  
  if (!missing(weights) && !is.null(weights) && !all.equal(sum(weights), nrow(data))) {
    warning(sprintf("The sum of the weights vector should equal the number of observations (%0.2f vs %d)", sum(weights), nrow(data)))
  }
  
  
}

#' Check the inputs to the draw function
#' 
#' @param draws The specified Model
#' @param nDraws Named vector of proposed start values for the model
#' @param draw_dimensions the dataset on which to estimate
#' @param Nindividuals The availabilities for the alternatives in the model specification
#' @return A list consisting of the checked draws and Ndraws, both computed if required)
#' 
#' 
check_draw_inputs <- function(draws, nDraws, draw_dimensions, Nindividuals) {
  if (missing(draws) && missing (nDraws)) {
    stop ("Either a draw matrix or the desired number of draws needs to be specified")  
  } else if (missing(draws) && !missing(nDraws)) {
    draws <- create_halton_draws(Nindividuals, nDraws, draw_dimensions)
    message(sprintf("Created a draw matrix of dimensions (%d, %d) for %d Individuals", nDraws, draw_dimensions, Nindividuals) )
  } else if (!missing(draws) && missing(nDraws)) {
    nDraws <- as.integer(nrow(draws) / Nindividuals) #get the maxmimum possible number of draws available
  } else if (!is.matrix(draws)) {
    stop ("The draw parameter provieded is not a matrix")
  }
  
  if (ncol(draws) < draw_dimensions || nrow(draws) < Nindividuals) {
    stop (sprintf("The draw matrix of dimensions %d x %d is not large enough (must be at least %d x %d)", nrow(draws), ncol(draws), Nindividuals, draw_dimensions))
  } 
  return (list('draws'=draws, 'nDraws'=nDraws))
}
