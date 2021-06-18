#' Calculate base variables impact (function for users)
#'
#' @param paths - paths to files with data
#' @param weights - weights for importance of variables in each model (default: NULL, equal)
#'
#' @return dataframe with columns: variable (base variable), impact (calculated based on several models specified in mdoel_ids), model_id
#'
#' @import readxl
#' @import dplyr
#'
#' @export
#'


baseImpact <- function(paths, weights = NULL){

  data = list()
  for(i in 1:length(paths)){
    calc_importance = calculate_base_variables_importance(importance_frame = readxl::read_excel(paths[i]), weights = weights)
    data[[i]] = calc_importance


    if(length(calc_importance) == 1){
      if(calc_importance == 0){
        return("Weights have to sum up to 1")
      } else if(calc_importance == 1){
        return(paste0("Wrong number of weights is specified for path: ", i))
      }
    }

  }
  data <- do.call(rbind.data.frame, data)

  return(data)
}
