#' Calculate combined model basic variables impact
#'
#' @param importance_frame - dataframe with columns: variable (base variable), impact, model_id
#' @param model_ids - id of component models
#' @param ensamble_model_id - id for combined model
#' @param weights - weights for importance of variables in each model (default: NULL, equal)
#'
#' @return res - dataframe with columns: variable (base variable), impact (calculated based on several models specified in mdoel_ids), model_id

calculate_combined_model <- function(importance_frame, model_ids, ensamble_model_id, weights = NULL){

  #Verify whether the user has entered the weights and their correct number
  if(is.null(weights)){
    weights = rep(1,length(model_ids))/length(model_ids)
  } else if(length(model_ids) != length(weights)){
    return("Pass correct number of weights")
  }

  #Select specified model id's
  importance_frame = importance_frame %>%
    filter(model_id %in% model_ids)

  variables <- importance_frame$variable %>% unique()
  res = data.frame("variable" = character(), "impact" = character(), "model_id" = numeric())

  #Calculate base variables impact for combined model
  for(variable in variables){
    temp_impact = importance_frame$impact[importance_frame$variable == variable]
    if(length(temp_impact) == 1){
      res = rbind(res, data.frame("variable" = variable, "impact" = temp_impact, "model_id" = ensamble_model_id))
    } else{
      res = rbind(res, data.frame("variable" = variable, "impact" = sum(temp_impact * weights), "model_id" = ensamble_model_id))
    }
  }

  return(res)
}

