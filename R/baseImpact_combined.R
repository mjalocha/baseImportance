#' Calculate base variables importance for combined model (function for users)
#'
#' @param paths - paths to files with data
#' @param model_ids - id of component models
#' @param ensamble_model_ids - combined model id
#' @param weights_metrics - weights for importance of variables in each model (default: NULL, equal)
#' @param weights_models - weights for importance of models impact (default: NULL, equal)
#' @param divide_by_number - boolean, specify weather do you want to divide impact by number of base variables
#'
#' @return dataframe with columns: variable (base variable), impact (calculated based on several models specified in mdoel_ids), model_id
#'
#' @import readxl
#' @import dplyr
#'
#' @export
#'


baseImpact_combined <- function(paths, model_ids, ensamble_model_id, weights_metrics = NULL, weights_models = NULL, divide_by_number = FALSE){
  data = list()
  for(i in 1:length(paths)){
    data[[i]] = calculate_base_variables_importance(importance_frame = read_excel(paths[i]), weights = weights_metrics, divide_by_number = divide_by_number)
  }
  data <- do.call(rbind.data.frame, data)

  return(calculate_combined_model(importance_frame = data, model_ids = model_ids, ensamble_model_id = ensamble_model_id, weights = weights_models))
}
