
#' Return the the utilities for a set of coefficients
#' 
#' @param model_spec The generated model_spec.
#' @param beta The coefficients to use in the model when estimating the utilities.
#' @param data The dataframe of observations.
#' @param availabilities The availabilities of each alternative.
#' @param draws For mixed models, a matrix of draws. If none is provided, one is created.
#' @param nDraws The number of draws to use or generated.
#' 
#' @return Dataframe of utilties for each observation
#' 
#' @example R/examples/utilities.R
#' 
#' @export 
utilities <- function(model_spec, beta, data, availabilities, draws, nDraws) {
  
  check_inputs(model_spec, beta, data, availabilities, draws, c(), NULL)
  
  template_filename <- "utilities.cpp"

  utility_template <- readr::read_file(system.file("include", "mixl", template_filename, package = "mixl"))

  Nindividuals <- length(unique(data$ID))
  
  if (model_spec$is_mixed) { # we only want to pass the draws through if the loglik function is mixed
    list2env(check_draw_inputs(draws, nDraws, model_spec$draw_dimensions, Nindividuals), envir = environment())
  } else {
    nDraws = 1
    draws = NULL
  }
  
  cpp_code <- convert_to_valid_cpp(utility_template, e1=model_spec)
  
  openmp_setting_file <- system.file(package = "mixl", "include", 'MIXL_OPENMP_FLAG')
  openmp_setting <- trimws(readChar(openmp_setting_file, file.info(openmp_setting_file)$size))
  
  Sys.setenv("PKG_CPPFLAGS"= sprintf("-I\"%s\"", system.file(package = "mixl", "include")))
  if (length(openmp_setting) > 0) {
    Sys.setenv("PKG_CXXFLAGS"= paste(openmp_setting, Sys.getenv("PKG_CXXFLAGS") ))
    Sys.setenv("PKG_LIBS"= paste(openmp_setting, Sys.getenv("PKG_LIBS") ))
  }

  f_env <- new.env()
  Rcpp::sourceCpp(code=cpp_code, env = f_env)
  
  util_matrix_names <- c("ID", switch(model_spec$is_mixed, "draw", NULL ), model_spec$utility_function_names)
  
  utilities_df = data.frame(f_env$utilities(beta, data, Nindividuals, availabilities, draws, nDraws))
  colnames(utilities_df) <- util_matrix_names

  return (utilities_df)
}