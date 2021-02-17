
#' Calculate the posteriors for a specified and estimated model
#' 
#' @param model The estimated Model
#' @param indiv_data Alternative individual data to use insteaf of that in the dataset
#' @param code_output_file An (optional) location where the compiled code should be saved (useful for debugging
#' 
#' @return Dataframe of individual-level posteriors
#' 
#' @example R/examples/posteriors.R
#' 
#' @export 
posteriors <- function(model, indiv_data, code_output_file=NULL) {
  
  data_cols <- extract_var(paste(model$rnd_equations[,'equation'], collapse="\n"),data_pattern)
  if (length(data_cols) == 0) data_cols <- NULL
  
  if (missing(indiv_data) || is.null(indiv_data)) {
    indiv_data <- extract_indiv_data(model$data, data_cols)
  }
  
  nDraws = ifelse(is.null(model$nDraws), 1, model$nDraws)
  
  f <- compile_posterior_function(model$rnd_equations, names(model$estimate), model$is_mixed, code_output_file)

  f(model$estimate, model$probabilities,
    model$Nindividuals, indiv_data,
    model$draws, nDraws)
}


parse_equations <- function(utility_script) {
  random_regex <- "\\b(\\w*_RND)\\s*=\\s*([^;]*)"
  a  <- stringr::str_match_all(utility_script, random_regex)
  a <- a[[1]][,c(2,3),drop=F]
  if (!is.null(a) && length(dim(a)) >= 2) colnames(a) <- c("name", "equation")
  a
}


compile_posterior_function <- function(rnd_equations, betas, is_mixed, output_file=NULL) {
  
  #posterior_template <- readr::read_file("inst/include/mixl/cpp_posteriors.cpp")
  template_filename <- "posteriors.cpp"
  
  posterior_template <- readr::read_file(system.file("include", "mixl", template_filename, package = "mixl"))
  
  
  #rnd_equations <- model$rnd_equations
  names <- rnd_equations[,"name"]
  equations <- rnd_equations[,'equation']
  
  beta_var_init_text <- 'double {beta_name} = betas["{beta_name}"];'
  beta_inits_vec <- sapply(betas, function (beta_name) stringr::str_glue(beta_var_init_text)) #vector creation
  beta_declarations <- paste(beta_inits_vec, collapse="\n")
  
  data_cols <- extract_var(paste(equations, collapse="\n"),data_pattern)
  data_var_init_text <- 'const NumericVector {data_prefix}{col_name} = data["{col_name}"];'
  data_inits_vec <- sapply(data_cols, function (col_name) stringr::str_glue(data_var_init_text)) #vector creation
  data_declarations <- paste(data_inits_vec, collapse="\n")
  
  data_subs <- stats::setNames (paste0(data_prefix, data_cols , "[i]"), paste0("\\$", data_cols, "\\b"))

  num_rnd_vars <- length(names)
  
  col_names <- paste0("colnames1[i++] = \"", names, "\";", collapse = "\n")
  
  random_paramters <- (paste0("indiv_B_means(i, rnd_idx++) += probabilities(i,d) * (", equations, ");", collapse="\n"))
  
  MIXED_MNL = ifelse(is_mixed, '#define _MIXED_MNL 1', '')
                       
  code <- stringr::str_glue(posterior_template, .open="!===", .close="===!")
  
  ccode_w_data  <- stringr::str_replace_all(code, data_subs)
  
  if (!is.null(output_file)) {
    readr::write_file(ccode_w_data, output_file)
  }
  
  f_env <- new.env()
  Rcpp::sourceCpp(code=ccode_w_data, env = f_env)
  
  return (f_env$mixl_posteriors)
  
  }
