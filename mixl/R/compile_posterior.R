
#' @export 
posteriors <- function(model) {
  f <- compile_posterior_function(model$rnd_equations)
  
  f(model$est, model$probabilities,
    model$Nindividuals,
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

  names <- rnd_equations[,"name"]
  equations <- rnd_equations[,'equation']

  num_rnd_vars <- length(names)
  
  col_names <- paste0("colnames1[i++] = \"", names, "\";", collapse = "\n")
  
  random_paramters <- (paste0("indiv_B_means(i, rnd_idx++) += probabilities(i,d) * (", equations, ");", collapse="\n"))
                       
  code <- stringr::str_glue(posterior_template, .open="!===", .close="===!") 
  
  f_env <- new.env()
  Rcpp::sourceCpp(code=code, env = f_env)
  
  return (f_env$mixl_posteriors)
  
  }