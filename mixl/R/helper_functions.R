#' @export
generate_default_availabilities <- function(data, num_utility_functions) {
  matrix(1, nrow(data), num_utility_functions)
}