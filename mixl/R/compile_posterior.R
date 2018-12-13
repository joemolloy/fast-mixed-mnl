
#' @export 
compile_posterior_function<- function(utility_script) {
  
  #posterior_template <- readr::read_file("inst/include/mixl/cpp_posteriors.cpp")
  posterior_template <- readr::read_file(system.file("include", "mixl", "cpp_posteriors.cpp", package = "mixl"))
  
  
  mnl_test <- "
      ASC_A_RND 	= @ASC_A 	+ draw_1 * @SIGMA_A1 		+ draw_7 * @SIGMA_A2;
      ASC_B_RND 	= @ASC_B 	+ draw_2 * @SIGMA_B;
      ASC_B 	= @ASC_B 	+ draw_2 * @SIGMA_B;
  
      U_A = ASC_A_RND + @B_price * $price_A / 1000 + @B_time * $time_A / 60 + @B_change * $change_A; 
      U_B = ASC_B_RND + @B_price * $price_B / 1000 + @B_timeB * $time_B / 60;
    "
  
  random_regex <- "\\b(\\w*_RND)\\s*=\\s*([^;]*)"
  a  <- stringr::str_match_all(mnl_test, random_regex)
  
  draw_prefix = "draw" #TODO: ecclu
  draw_pattern    <-  sprintf("%s\\w+\\b", draw_prefix)
  draws <- unique(stringr::str_extract_all(mnl_test,draw_pattern)[[1]])
  draw_dimensions = length(draws)
  
  names <- a[[1]][,2]
  equations <- a[[1]][,3]
  
  draw_subs <-  setNames (paste0("draw[", 0:(draw_dimensions-1), "]"), paste0(draws,"\\b"))
  equations1 <- stringr::str_replace_all(equations, draw_subs)
  
  beta_pattern <- "@(\\w+)\\b"
  equations2 <- stringr::str_replace_all(equations1, beta_pattern, "betas[\"\\1\"]")
  
  num_rnd_vars <- length(names)
  
  col_names <- paste0("colnames1[i++] = \"", names, "\";", collapse = "\n")
  
  random_paramters <- (paste0("indiv_B_means(i, rnd_idx++) += probabilities(i,d) * (", equations2, ");", collapse="\n"))
                       
  code <- stringr::str_glue(posterior_template, .open="!===", .close="===!")         
  
  Rcpp::sourceCpp(code=code)
  
  return (code)
  
  }