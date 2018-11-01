reserved_words = c("utilities") #TODO: ecclu

beta_pattern <- "@(\\w+)"
draw_pattern <- "draw(\\d+)"
data_pattern <- "\\$(\\w+)"
new_vars_pattern <- "^\\w+"

extract_new_vars <- function (var_lines) {

  script_new_vars <- sapply(var_lines, function (s) stringr::str_extract_all(s, new_vars_pattern)[[1]])
  script_new_vars <- setdiff(script_new_vars, reserved_words)
  return (script_new_vars)
}

trim_marker <- function(s) stringr::str_sub(s, 2)

extract_var <- function(text, pattern) {

  script_els = stringr::str_extract_all(text,pattern)[[1]]
  unqiue_els = unique(script_els)
  return (sapply(unqiue_els, trim_marker))
}

extract_variables <- function (source_txt) {
  e <- new.env()
  e$source <- source_txt

  script_lines <- stringr::str_split(source_txt, "\n")[[1]]
  #filter out comment lines
  code_lines <-  script_lines[
    !startsWith(script_lines, "//") &
      !startsWith(script_lines, "#")  &
      nchar(script_lines) > 0
    ]

  e$code_lines <- code_lines

  e$var_lines <- code_lines[!startsWith(code_lines, "utilities") & grepl("\\w", code_lines)]
  e$util_lines <- code_lines[startsWith(code_lines, "utilities")]

  e$new_vars <- extract_new_vars(e$var_lines)
  e$draws <- unique(stringr::str_extract_all(source_txt,draw_pattern)[[1]])
  e$draw_dimensions <- length(e$draws)

  e$data_cols = extract_var(source_txt,data_pattern)
  e$betas = extract_var(source_txt,beta_pattern)

 return (e)

}

validate_env <- function (e1, data_names, beta_names) {

  e1$data_errors <- setdiff(e1$data_cols, data_names)
  e1$beta_errors <- setdiff(e1$betas, beta_names)

  valid = TRUE
  data_msg = c()
  beta_msg = c()

  e1$errors = list()

  if (length(e1$data_errors) > 0) {
    valid = FALSE
    data_msg <- paste("The following variables are not available in the dataset:", e1$data_errors)
  }

  #check betas are all in the beta list
  if (length(e1$beta_errors) > 0) {
    valid = FALSE
    beta_msg <- paste("The following coefficients are not named:", e1$beta_errors)
  }

  #check highest draw number is lower than requested
  #TODO

  e1$is_valid <- valid
  e1$error_messages <- c(data_msg, beta_msg)

  return (valid)

}
#TODO: decide if draws should be changed everywhere!



convert_to_valid_cpp <- function(cpp_template, e1) {

  data_prefix <- "data_"
  data_sub <- stringr::str_glue("{data_prefix}\\1[i]")
  draw_sub <- "draw[\\1]"
  
  draw_sub2 <- setNames(c(0:e1$draw_dimensions-1), e1$draws)
  
  d_sub_f <- function(draw_name) {
    paste0("draw[", draw_sub2[draw_name], "]")
  }

  #build data column vector initialization code
  data_var_init_text <- 'const NumericVector {data_prefix}{col_name} = data["{col_name}"];'
  data_inits_vec <- sapply(e1$data_cols, function (col_name) stringr::str_glue(data_var_init_text)) #vector creation
  data_declarations <- paste(data_inits_vec, collapse="\n")

  #build betas initialization code
  beta_var_init_text <- 'double {beta_name} = beta1["{beta_name}"];'
  beta_inits_vec <- sapply(e1$betas, function (beta_name) stringr::str_glue(beta_var_init_text)) #vector creation
  beta_declarations <- paste(beta_inits_vec, collapse="\n")


  #add double type to new var initialization
  var_initialisations_list <- lapply(e1$var_lines, function(s) paste("double", s))

  ccode <- c(var_initialisations_list, c("\n"), e1$util_lines)
  #replace data and draws
  script_w_data <- stringr::str_replace_all(ccode, data_pattern, data_sub)
  script_w_data_draws <- stringr::str_replace_all(script_w_data, draw_pattern, d_sub_f)
  script_wo_ats <- stringr::str_replace_all(script_w_data_draws, beta_pattern, "\\1")
  draw_and_utility_declarations <- paste(script_wo_ats, collapse="\n")

  #fill in template
  cpp_code <- stringr::str_glue(cpp_template, .open="!===", .close="===!")

  return (cpp_code)

}


#' @export
preprocess_file <- function (utility_script, cpp_template, data, beta, output_file = NULL) {

  data_names <- names(data)
  beta_names <- names(beta)

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
compileUtilityFunction <- function( script, data, betas ) {
  header_file_location <- system.file("include", "mixl", "cpp_utility_template.h", package = "mixl")
  cpp_template <- readr::read_file(header_file_location)
  e1 <- mixl::preprocess_file(script, cpp_template, data, betas)
  Rcpp::sourceCpp(code = e1$cpp_code)
  return (e1)
  
}

#load("../checkpoint.RData")

#cpp_template <- readr::read_file("../run_mixl/cpp_utility_template.cpp")
#utility_script <- readr::read_file("../run_mixl/utility_script.txt")
#output_file <- "../run_mixl/cpp_utility_processed.cpp"

#preprocess_file(utility_script, cpp_template, output_file, data, beta)






