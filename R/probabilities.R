compile_predictions <- function(model_spec) {
  prediction_template <- if (model_spec$is_mixed) "cpp_mixed_prediction_template.h" else "cpp_mnl_prediction_template.h"
  
  template_location <- system.file("include", "mixl", prediction_template, package = "mixl")
  cpp_template <- readr::read_file(template_location)
  
  cpp_code <- convert_to_valid_cpp(cpp_template, e1=model_spec)
  
  f_env <- new.env()
  
  Sys.setenv("PKG_CPPFLAGS"= sprintf("-I\"%s\"", system.file(package = "mixl", "include")))
  Rcpp::sourceCpp(code = cpp_code, env = f_env)

  return (f_env$predict)    
}



#' Calculate the probabilities for a specified and estimated model. 
#' Note that if new data or draws are provided, the model will not be re-estimated
#' 
#' @param model The estimated Model
#' @param data (Optional) New data to use instead of that in the dataset
#' @param availabilities (Optional) New availabilites to use
#' @param draws (Optional) Optional new set of random draws to use
#' @param nDraws (Optional) Optional new number of random draws to use
#' @param num_threads Enable parallel computing where available using this many cores
#' 
#' @return Dataframe of individual-level posteriors
#' 
#' @example R/examples/probabilities.R
#' 
#' @export 
probabilities <- function(model, 
                          data=NULL, availabilities = NULL, 
                          draws = NULL, nDraws = NULL, 
                          num_threads=1) {
  
  #TODO: check draws and data for size
  if (missing(data) | is.null(data)) {
    new_data <- model$data
    new_Nindividuals <- model$Nindividuals
  } else {
    new_data <- data
    new_Nindividuals = length(unique(new_data$ID))
  }
  
  if (missing(availabilities) | is.null(availabilities)) {
    new_availabilities <- model$availabilities
  } else {
    new_availabilities <- availabilities
    
  }
  
  
  if (nrow(new_data) != nrow(new_availabilities)) {
    stop("The number of rows in the supplied data and availabilities are not the same")
  }
  
  if (model$model_spec$num_utility_functions > ncol(new_availabilities)) {
    stop("The number of columns in the availabilities is less than the number of utility functions")
  }
  
  data_errors <- setdiff(model$model_spec$data_cols, colnames(new_data))
  
  if (length(data_errors) > 0) {
    stop(paste("The following variables are not available in the dataset:", paste(data_errors, collapse = ", ")))
  }
  
  
  #################################
  
  f <- compile_predictions(model$model_spec)

  #handle basic mnl case without and draws
  if(!model$is_mixed) {

    f(model$estimate, new_data, new_Nindividuals, new_availabilities, num_threads)
    
  } else { 

    if (missing(draws) | is.null(draws)) {
      new_draws <- model$draws
      new_nDraws <- model$nDraws
    } else {
      new_draws <- draws
      new_nDraws <- nDraws
    }
    
    if (ncol(new_draws) < model$model_spec$draw_dimensions) {
      stop(paste0("Not enough columns in draw matrix. Need ", model$model_spec$draw_dimensions))
    }
    
    if (nrow(new_draws) < new_Nindividuals * new_nDraws) {
      stop(paste0("Not enough rows in draw matrix. Need ", model$model_spec$draw_dimensions))
    }   
    
    f(model$estimate, new_data,
      new_Nindividuals, new_availabilities, new_draws, new_nDraws, num_threads)
  }
}

