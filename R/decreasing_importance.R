#' Calculate decreasing importance
#'
#' @param base_imp - highest importance for specified variable
#' @param rest_imp - the rest of importances for specified variable
#'
#' @return sum_imp - numeric value with base variable importance


decreasing_importance <- function(base_imp, rest_imp){
  sum_imp = base_imp
  n = 2
  for(imp in rest_imp %>% unlist() %>% .[rest_imp %>% order(decreasing = TRUE)]){
    sum_imp = sum_imp + imp/n
    n = n + 1
  }

  return(sum_imp)
}
