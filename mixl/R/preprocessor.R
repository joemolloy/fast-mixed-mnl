utility_prefix = "U_" #TODO: ecclu
p_indic_prefix = "P_indic_" #TODO: ecclu

beta_pattern <- "@(\\w+)\\b"
draw_pattern <- "draw(\\d+)\\b"
data_pattern <- "\\$(\\w+)\\b"
new_vars_pattern <- sprintf("\\b((?!%s)\\w+)\\b(?=\\s*\\=)", utility_prefix)
utility_pattern <-  sprintf("%s\\w+\\b", utility_prefix)
p_indic_pattern <-  sprintf("%s\\w+\\b", p_indic_prefix)


extract_new_vars <- function (var_lines) {

  script_new_vars <- sapply(var_lines, function (s) stringr::str_extract_all(s, new_vars_pattern)[[1]])
  return (script_new_vars)
}

trim_marker <- function(s) stringr::str_sub(s, 2)

extract_var <- function(text, pattern) {

  script_els = stringr::str_extract_all(text,pattern)[[1]]
  unqiue_els = unique(script_els)
  return (sapply(unqiue_els, trim_marker))
}

replace_orm <- function(text) {
  dnorm
}

extract_variables <- function (source_txt) {
  e <- new.env()
  e$source <- source_txt
  
  source_wo_comments <- stringr::str_replace_all(e$source, "(?://|#).*", ""); #remove all comment lines

  #any lines dont start with U_ are variable definition lines
  e$new_vars <- unique(stringr::str_extract_all(source_wo_comments,new_vars_pattern))[[1]]

  e$utility_function_names = unique(stringr::str_extract_all(source_wo_comments, utility_pattern)[[1]])
  e$num_utility_functions = length(e$utility_function_names)

  e$draws <- unique(stringr::str_extract_all(source_wo_comments,draw_pattern)[[1]])
  e$draw_dimensions <- length(e$draws)

  e$data_cols = extract_var(source_wo_comments,data_pattern)
  e$betas = extract_var(source_wo_comments,beta_pattern)
 
  #get p_indic filenames for hybrid choice if they are available
  e$p_indics <- e$new_vars[startsWith(e$new_vars, p_indic_prefix)]
  e$is_hybrid_choice <- length(e$p_indics) > 0 
  
 return (e)

}

create_p_indic_sum <- function(p_indics) {
  paste("P_indic_total = ", paste(p_indics, collapse = " * "), ";")
}

validate_env <- function (e1, data_names, beta_names) {

  e1$data_errors <- setdiff(e1$data_cols, data_names)
  
  e1$beta_errors <- setdiff(e1$betas, beta_names)
  e1$excess_betas <- setdiff(beta_names, e1$betas)
  
  e1$new_var_errors <- intersect(e1$new_vars, beta_names)

  valid = TRUE
  data_msg = c()
  beta_msg = c()
  new_var_msg = c()
  
  e1$errors = list()

  if (length(e1$data_errors) > 0) {
    valid = FALSE
    e1$error_messages <- c(e1$error_messages, paste("The following variables are not available in the dataset:", paste(e1$data_errors, collapse = ", ")))
  }

  #check betas are all in the beta list
  if (length(e1$beta_errors) > 0) {
    valid = FALSE
    e1$error_messages <- c(e1$error_messages, paste("The following parameters are not named:", paste(e1$beta_errors, collapse = ", ")))
  }
  
  if (length(e1$new_var_errors) > 0) {
    valid = FALSE
    e1$error_messages <- c(e1$error_messages, paste("The following new variables have the same names as parameters:", paste(e1$new_var_errors, collapse = ", ")))
  }
  
  if (length(e1$excess_betas) > 0) {
    for (b in e1$excess_betas) {
      warning(paste("The following parameter was not used in the utility function but will be estimated anyway:", b))
    }
  }
  

  e1$is_valid <- valid

  return (valid)

}

add_semi_colons <- function(lines) {
  lapply(X = lines, FUN = function(l) {
    if (endsWith(l, ";")) l else paste0(l, ";")
  })
}

convert_to_valid_cpp <- function(cpp_template, e1, hybrid_choice=FALSE) {

  data_prefix <- "data_"
  
  data_subs <- setNames (paste0(data_prefix, e1$data_cols , "[i]"), paste0("\\$", e1$data_cols, "\\b"))
  draw_subs <-  setNames (paste0("draw[", 0:(e1$draw_dimensions-1), "]"), paste0(e1$draws,"\\b"))
  utility_subs <- setNames (paste0("utilities[", 0:(e1$num_utility_functions-1) , "]"), paste0(e1$utility_function_names, "\\b"))

  #build data column vector initialization code
  data_var_init_text <- 'const NumericVector {data_prefix}{col_name} = v.data["{col_name}"];'
  data_inits_vec <- sapply(e1$data_cols, function (col_name) stringr::str_glue(data_var_init_text)) #vector creation
  data_declarations <- paste(data_inits_vec, collapse="\n")

  #build betas initialization code
  beta_var_init_text <- 'double {beta_name} = beta1["{beta_name}"];'
  beta_inits_vec <- sapply(e1$betas, function (beta_name) stringr::str_glue(beta_var_init_text)) #vector creation
  beta_declarations <- paste(beta_inits_vec, collapse="\n")


  #add double type to new var initialization
  ccode_w_var_init <- stringr::str_replace_all(e1$source, new_vars_pattern, "double \\1")

  #replace util prefixes
  ccode_w_utils <- stringr::str_replace_all(ccode_w_var_init, utility_subs)
  
  #replace data and draws
  ccode_w_data  <- stringr::str_replace_all(ccode_w_utils, data_subs)
  ccode_w_draws <- stringr::str_replace_all(ccode_w_data, draw_subs)
  ccode_wo_ampersands <- stringr::str_replace_all(ccode_w_draws, beta_pattern, "\\1")
  #sript_with_semicolons <- add_semi_colons(script_wo_ats)
  draw_and_utility_declarations <- ccode_wo_ampersands
  
  #number of utilities, take from the number of utility lines
  utility_length = e1$num_utility_functions
  
  #summing p_indics for hybrid choice
  prob_indicator_sum <- if (e1$is_hybrid_choice) create_p_indic_sum(e1$p_indics) else "//note: not a hybrid choice model"

  #fill in template
  cpp_code <- stringr::str_glue(cpp_template, .open="!===", .close="===!")

  return (cpp_code)

}


#' @export
preprocess_file <- function (utility_script, cpp_template, data, betas, output_file = NULL) {

  data_names <- names(data)
  beta_names <- names(betas)

  e1 = extract_variables(utility_script)

  validate_env(e1, data_names, beta_names)
  
  if (e1$is_valid) { #start making the replacements

    cpp_code <- convert_to_valid_cpp(cpp_template, e1=e1)
    e1$cpp_code <- cpp_code

    if (!is.null(output_file)) {
      readr::write_file(cpp_code, output_file)
    }
    return (e1)

  } else {
    stop (paste(c("The utility script is not valid", e1$error_messages), collapse = "\n"))
  }
}

#' @export
compileUtilityFunction <- function( script, data, betas , output_file = NULL, compile=TRUE) {
  cpp_container <- new.env()
  cpp_container$logLik <- NULL ## remove old function
  
  header_file_location <- system.file("include", "mixl", "cpp_utility_template.h", package = "mixl")
  cpp_template <- readr::read_file(header_file_location)
  e1 <- mixl::preprocess_file(script, cpp_template, data, betas, output_file)
  
  cpp_container$num_utility_functions <- e1$num_utility_functions
  cpp_container$draw_dimensions <- e1$draw_dimensions
  
  if (compile) Rcpp::sourceCpp(code = e1$cpp_code, env = cpp_container)
  
  return (cpp_container)
  
}

#load("../checkpoint.RData")

#cpp_template <- readr::read_file("../run_mixl/cpp_utility_template.cpp")
#utility_script <- readr::read_file("../run_mixl/utility_script.txt")
#output_file <- "../run_mixl/cpp_utility_processed.cpp"

#preprocess_file(utility_script, cpp_template, output_file, data, beta)






