#' @export
generate_default_availabilities <- function(data, num_utility_functions) {
  matrix(1, nrow(data), num_utility_functions)
}

#' @export
create_halton_draws <- function(Nindividuals, Ndraws, draw_dimensions) {
  randtoolbox::halton(Nindividuals*Ndraws, draw_dimensions, normal=TRUE)
}