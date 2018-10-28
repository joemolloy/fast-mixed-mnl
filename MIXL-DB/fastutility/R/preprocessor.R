library(stringr)
library(readr)

utility_script <- readr::read_file("../run_mixl/utility_script.txt")

comment_regex = "/\\*+[^*]*\\\\*+(?:[^/*][^*]*\\\\*+)*/";
without_comments <- stringr::str_replace(utility_script, comment_regex,"")
cat(without_comments)
text <- "B_TT_W_RND  = ( B_TT_W + draw1 * SIGMA_TT_W );"

reserved_words = c("utilities", "draw", "exp", "log") #TODO: ecclu

beta_pattern <- "@(\\w+)"
draw_pattern <- "draw(\\d+)"
data_pattern <- "\\$(\\w+)"

new_vars_pattern <- "^\\w+"

data_text <- "aaaa $test_col eee"

data_names <- names(data)
beta_names <- names(beta)

#get new vars - they need to be excluded, and then prepended with double later
script_lines <- str_split(utility_script, "\n")[[1]]
#filter out comment lines and rejoin
code_lines <-  script_lines[
  !startsWith(script_lines, "//") &
    !startsWith(script_lines, "#")  &
    nchar(script_lines) > 0
]

var_lines <- code_lines[!startsWith(code_lines, "utilities")]
util_lines <- code_lines[startsWith(code_lines, "utilities")]

#get new declared vars in script
script_new_vars = sapply(var_lines, function (s) str_extract_all(s, new_vars_pattern)[[1]])
script_new_vars = setdiff(script_new_vars, reserved_words)
script_new_vars

script_draws = str_extract_all(utility_script,draw_pattern)[[1]]

trim_marker <- function(s) stringr::str_sub(s, 2)
get_variables <- function(text, pattern) {
  script_els = str_extract_all(text,pattern)[[1]]
  unqiue_els = unique(script_els)
  return (sapply(unqiue_els, trim_marker))
}

#check data_els are in the data list
data_els1 = get_variables(utility_script,data_pattern)
script_betas = get_variables(utility_script,beta_pattern)

data_errors <- setdiff(data_els1, data_names)
beta_errors <- setdiff(script_betas, beta_names)


#need to capture script betas which aren't anything else






valid = TRUE

if (length(data_errors) > 0) {
  valid = FALSE
  print("The following variables are not available in the dataset:")
  data_errors
}

#check betas are all in the beta list
if (length(beta_errors) > 0) {
  valid = FALSE
  print("The following coefficients are not named:")
  beta_errors
}

#check highest draw number is lower than requested

if (valid) { #start making the replacements

  col_name = "test_col"
  data_prefix <- "data_"
  data_sub <- str_glue("{data_prefix}\\1[i]")
  draw_sub <- "draw[\\1]"

  #build data column vector initialization code
  data_var_init_text <- 'const NumericVector {data_prefix}{col_name} = data["{col_name}"];'
  data_inits_vec <- sapply(data_els1, function (col_name) str_glue(data_var_init_text)) #vector creation
  data_declarations <-stringi::stri_paste(data_inits_vec, collapse="\n")

  #build betas initialization code
  beta_var_init_text <- 'double {beta_name} = beta1["{beta_name}"];'
  beta_inits_vec <- sapply(script_betas, function (beta_name) str_glue(beta_var_init_text)) #vector creation
  beta_declarations <- stringi::stri_paste(beta_inits_vec, collapse="\n")


  #add double type to new var initialization
  var_initialisations_list <- lapply(var_lines, function(s) paste("double", s))

  ccode <- c(var_initialisations_list, util_lines)
  #replace data and draws
  script_w_data <- str_replace_all(ccode, data_pattern, data_sub)
  script_w_data_draws <- str_replace_all(script_w_data, draw_pattern, draw_sub)
  script_wo_ats <- str_replace_all(script_w_data_draws, beta_pattern, "\\1")
  draw_and_utility_declarations <- stringi::stri_paste(script_wo_ats, collapse="\n")



  #write out result
  cpp_template <- readr::read_file("../run_mixl/cpp_utility_template.cpp")
  r <- stringr::str_glue(cpp_template, .open="!===", .close="===!")

  output_file <- "../run_mixl/cpp_utility_processed.cpp"
  readr::write_file(r, output_file)
  Rcpp::sourceCpp(output_file)
getwd()

}




