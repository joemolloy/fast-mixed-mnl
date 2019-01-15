
#' @export 
posteriors <- function(model) {
  f <- compile_posterior_function(model$rnd_equations)
  
  indiv_data <- extract_indiv_data(model$data)
  
  f(model$est, model$probabilities,
    model$Nindividuals, indiv_data,
    model$draws, model$nDraws)
}

#' @export 
parse_equations <- function(utility_script) {
  random_regex <- "\\b(\\w*_RND)\\s*=\\s*([^;]*)"
  a  <- stringr::str_match_all(utility_script, random_regex)
  a <- a[[1]][,c(2,3),drop=F]
  if (!is.null(a) && length(dim(a)) >= 2) colnames(a) <- c("name", "equation")
  a
}


compile_posterior_function <- function(rnd_equations) {
  
  #posterior_template <- readr::read_file("inst/include/mixl/cpp_posteriors.cpp")
  posterior_template <- readr::read_file(system.file("include", "mixl", "cpp_posteriors.cpp", package = "mixl"))

  #rnd_equations <- model$rnd_equations
  names <- rnd_equations[,"name"]
  equations <- rnd_equations[,'equation']
  
  data_cols <- extract_var(paste(equations, collapse="\n"),data_pattern)
  data_var_init_text <- 'const NumericVector {data_prefix}{col_name} = data["{col_name}"];'
  data_inits_vec <- sapply(data_cols, function (col_name) stringr::str_glue(data_var_init_text)) #vector creation
  data_declarations <- paste(data_inits_vec, collapse="\n")
  
  data_subs <- setNames (paste0(data_prefix, data_cols , "[i]"), paste0("\\$", data_cols, "\\b"))

  num_rnd_vars <- length(names)
  
  col_names <- paste0("colnames1[i++] = \"", names, "\";", collapse = "\n")
  
  random_paramters <- (paste0("indiv_B_means(i, rnd_idx++) += probabilities(i,d) * (", equations, ");", collapse="\n"))
                       
  code <- stringr::str_glue(posterior_template, .open="!===", .close="===!")
  
  ccode_w_data  <- stringr::str_replace_all(code, data_subs)
  
  
  f_env <- new.env()
  Rcpp::sourceCpp(code=ccode_w_data, env = f_env)
  
  return (f_env$mixl_posteriors)
  
  }