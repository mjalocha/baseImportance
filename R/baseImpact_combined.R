#' Calculate base variables importance for combined model (function for users)
#'
#' @param paths - paths to files with data
#' @param model_ids - id of component models (vector of numeric values)
#' @param ensamble_model_ids - combined model id
#' @param weights_metrics - weights for importance of variables in each model (default: NULL, equal)
#' @param weights_models - weights for importance of models impact (default: NULL, equal)
#'
#' @return dataframe with columns: variable (base variable), impact (calculated based on several models specified in mdoel_ids), model_id
#'
#' @import readxl
#' @import dplyr
#'
#' @export


baseImpact_combined <- function(paths, model_ids, ensamble_model_id, weights_metrics = NULL, weights_models = NULL){

  #Verify weather weights for models sum up to 1
  if(sum(weights_models) != 1 & !is.null(weights_models))
    return("Weights have to sum up to 1")

  #Verify weather correct number of weights is specified
  if(length(weights_models) != length(model_ids) & !is.null(weights_models))
    return("You need to specify weight for each model")

  data = list()
  number_of_cols = c()
  for(i in 1:length(paths)){
    file = read_excel(paths[i])
    number_of_cols = c(number_of_cols, ncol(file))
    calc_importance = calculate_base_variables_importance(importance_frame = file, weights = weights_metrics[[i]])
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

  #Verify weather specified model ids are available in the imported files
  if(model_ids %in% data$model_id %>% sum() != length(model_ids))
     return("Not all model_ids are available in the imported files")


  return(calculate_combined_model(importance_frame = data, model_ids = model_ids, ensamble_model_id = ensamble_model_id, weights = weights_models))
}
