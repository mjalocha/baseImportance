#' Opposite to %in% function
#'
#' @param x - vector with values that are in y
#' @param y - vector to verify weather there are values from x in it
#'
#' @return vector with values that are in x and not in y
#' @export
#'


'%!in%' <- function(x,y){!('%in%'(x,y))}
