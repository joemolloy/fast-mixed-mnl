#' @export
generate_default_availabilities <- function(data, num_utility_functions) {
  matrix(1, nrow(data), num_utility_functions)
}

#' @export
extract_av_cols <- function(data, prefix) {
  av <- names(data)
  av[sapply(FUN=function(x) startsWith(x, availability_prefix), av)]
}

#' @export
av_matrix <- function (data, av_cols) {
  as.matrix(data[, av_cols]) 
}

#' @export
create_halton_draws <- function(Nindividuals, nDraws, draw_dimensions) {
  as.matrix(randtoolbox::halton(Nindividuals*nDraws, draw_dimensions, normal=TRUE))
}