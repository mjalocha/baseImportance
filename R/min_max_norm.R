#' Normalize data
#'
#' @param x - vector with data to normalize
#'
#' @return normalzied vector
#'
#' @export

min_max_norm <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}
