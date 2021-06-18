#' Calculate class based on specific metric values
#'
#' @param ms - specific metric values (for example Information.gain)
#'
#' @return vector with classes
#'
#' @export

simple_quantiles <- function(ms){

  if( min(ms) > 0.95*max(ms)) return(rep(1, length(ms)))

  if(length(ms) < 8) return(dplyr::ntile(-(ms), 4))

  x <- dplyr::ntile(ms, 8)
  y <- dplyr::case_when(x == 8 ~ 1,
                        x > 5 ~ 2,
                        x > 3 ~ 3,
                        TRUE ~ 4)

  return(y)

}
