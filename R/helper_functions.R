#' Generate a ones-matrix of availabilities 
#' 
#' @param data The dataset used in the model
#' @param num_utility_functions the number of alternatives in the model
#' @return Ones-matrix of availabilities for alternatives and the number of choice observations
#' 
#' @example R/examples/generate_default_availabilities.R
#'  
#' @export
generate_default_availabilities <- function(data, num_utility_functions) {
  matrix(1, nrow(data), num_utility_functions)
}

#' Extract the availabilites matrix from the dataset using a column name prefix
#' 
#' @param data The dataset used in the model
#' @param prefix The prefix of the availability columns, i.e. avail_
#' @return Matrix of availabilities for alternatives and the number of choice observations
#' 
#' @example R/examples/extract_av_cols.R
#' 
#' @export
extract_av_cols <- function(data, prefix) {
  av <- names(data)
  cols = av[sapply(FUN=function(x) startsWith(x, prefix), av)]
  av_matrix(data, cols)
}

#' Extract the availabilites matrix from the dataset, using column indicies
#' 
#' @param data The dataset used in the model
#' @param av_cols A vector of the the column indicies of the availabilities for each alternative
#' @return Matrix of availabilities for alternatives and the number of choice observations
#' 
#' @example R/examples/av_matrix.R
#' 
#' @export
av_matrix <- function (data, av_cols) {
  as.matrix(data[, av_cols]) 
}

#' Create a standard set of Halton draws to use in estimation
#' 
#' @param Nindividuals The number individuals in the dataset
#' @param nDraws The number of draws needed
#' @param draw_dimensions the number of draw dimensions needed
#' @return Matrix of availabilities for alternatives and the number of choice observations
#' 
#' @examples 
#' create_halton_draws(100, 10, 5)
#' create_halton_draws(100, 100, 20)
#' 
#' @export
create_halton_draws <- function(Nindividuals, nDraws, draw_dimensions) {
  as.matrix(randtoolbox::halton(Nindividuals*nDraws, draw_dimensions, normal=TRUE))
}

#' Extract the individual level data from the dataset for use in posterior analysis
#' 
#' @param data The dataset
#' @param data_cols The individual level columns of attributes - Can be null to take aggregate for each column
#' @return dataframe of all individual level data for each ID
#' 
#' @example R/examples/extract_indiv_data.R
#' 
#' @export
extract_indiv_data <- function(data, data_cols = NULL) {
  #aggregate(x=data1, by=list(ID=data1$ID), FUN=mean)
  stats::aggregate(x=data[,c("ID", data_cols)], by=list(data$ID), FUN=mean)
}