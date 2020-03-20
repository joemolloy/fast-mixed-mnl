
test_for_openmp_osx <- function() {
  #if ((Sys.info()["sysname"] == 'Darwin')) {
  #    warning("You are on mac OSX")
  #}
}

#' Validate the utility functions against the dataset and generate the optimised logliklihood function
#' 
#' This function takes a utility function description, and generates a optimised C++ version 
#' of the utility function which can be called from R. If the data_names are provided, then the variables
#' in the function are checked against those provided. If an `output_file` is provided, the C++ code is saved there. 
#' See the user guide vignette for how to write valid utility scripts. There is some minimal specific syntax required.
#' 
#' @seealso browseVignettes("mixl")
#' 
#' @param utility_script The utility script to be compiled
#' @param dataset An (optional) dataframe to check if the all the variables are present
#' @param output_file An (optional) location where the compiled code should be saved (useful for debugging
#' @param compile If compile is false, then the code will not be compiled, but just validated and saved if an `output_file` is specified
#' @param model_name A name for the model, which will be used for saving. Defaults to *mixl_model*
#' @param disable_multicore True to disable openMP parallelism if openMP isn't installed (default on OSX - see the user guide for how to enable it)
#' @return An `object` which contains the loglikelihood function, and information from the compile process
#' 
#' @example R/examples/specify_model.R
#' 
#' @export 
specify_model <- function( utility_script, dataset = NULL , output_file = NULL, 
                           compile=TRUE, model_name="mixl_model", disable_multicore=F) {

  #TODO: if data is null, skip all the validaiton
  #TODO: return an object instead of an environment
  data_names <- names(dataset)
  
  if (!missing(data_names) & (is.character(data_names) & !is.vector(data_names))) {
    stop ( "data_names argument must either be NULL or a vector of column names") #TODO move this to compile
  }
  if (!all(c("ID", "CHOICE") %in% data_names)) stop ( "ID and CHOICE columns must be present in data") #TODO move this to compile
  
  e1 = extract_variables(utility_script)
  validate_env(e1, data_names)
  
  if (!e1$is_valid) { #start making the replacements
    stop (paste(c("The utility script is not valid", e1$error_messages), collapse = "\n"))
    
  } else{
    
    template <- if (e1$is_mixed) "cpp_utility_template.h" else "cpp_mnl_template.h"
    template_location <- system.file("include", "mixl", template, package = "mixl")
    cpp_template <- readr::read_file(template_location)
    
    e1$cpp_code <- convert_to_valid_cpp(cpp_template, e1=e1)

    if (!is.null(output_file)) {
      readr::write_file(e1$cpp_code, output_file)
    }
    
    cpp_container <- e1
    cpp_container$logLik <- NULL ## remove old function
    
    cpp_container$beta_names <- stats::setNames(e1$betas, NULL)
    cpp_container$num_coeffs <- length(e1$betas)
    
    #set modelname 
    cpp_container$model_name <- model_name
    
    if (compile) {
      
      openmp_setting <- ifelse(disable_multicore, "", "-fopenmp")
      Sys.setenv("PKG_CPPFLAGS"= sprintf("%s -I\"%s\"", openmp_setting, system.file(package = "mixl", "include")))
      
      tryCatch({
        Rcpp::sourceCpp(code = e1$cpp_code, env = cpp_container)
      }, error = function(e) {
        if ((Sys.info()["sysname"] == 'Darwin')) {
          warning("You are on mac OSX")
        }
      })
      

    }
    return (cpp_container)
  }
  
}

#' compileUtilityFunction
#' Deprecated, please see [specify_model()]
#' @param ... Parameters to specify_model
#'
#' @export
compileUtilityFunction <- function(...) {
  .Deprecated("specify_model")
  specify_model(...)
}


