#' Calculate base variables importance for each model
#'
#' @param importance_frame - dataframe with columns: model variable name, base variable name, model id, class, and the rest of the columns concern the impact of the variables
#' @param divide_by_number - boolean, do you want to divide impact by number of base variables
#' @param weights - weights for importance of variables in each model (default: NULL, equal)
#' @param base_columns - names for first four columns (default: c("model_variable", "base_variable", "model_id"))
#'
#' @return dataframe with columns: variable (base variable), impact (calculated based on several models specified in mdoel_ids), model_id
#'
#' @import dplyr
#' @import tidyr


calculate_base_variables_importance <- function(importance_frame, weights = NULL, base_columns = c("model_variable", "base_variable", "model_id"))
{

  #Set colnames
  colnames(importance_frame)[1:length(base_columns)] <- base_columns
  metric_cols = colnames(importance_frame)[colnames(importance_frame) %!in% base_columns]

  #Verify whether the user has entered the weights
  if(is.null(weights)){
    weights = rep(1,length(metric_cols))/length(metric_cols)
  }

  #Verify weather weights sum up to 1
  if(sum(weights) != 1)
    return(0)

  #Verify weather correct number of weights is specified
  if(length(metric_cols) != length(weights))
    return(1)

  #Change metrics type to numeric
  importance_frame[,metric_cols] <- sapply(importance_frame[,metric_cols],as.numeric)

  #Calculate impact for all model id's
  models = importance_frame$model_id %>% unique()

  res = list()
  for(model in models){
    #Add class to dataframe
    temp_frame = classify_variables(frame = importance_frame[importance_frame$model_id == model,], base_columns = base_columns, metric_columns = metric_cols)

    #Calculate impact
    temp_frame <- decreasing_frequency_importance(frame = temp_frame, base_columns = c(base_columns, "class"))
    temp_frame <- calculate_impact(base_variables_importance = temp_frame, metric_cols = metric_cols, weights = weights)
    temp_frame$model_id = model
    res[[paste(model)]] = temp_frame %>% select(variable, impact, class, model_id)
  }

  #Set correct rownames
  res = do.call(rbind.data.frame, res)
  rownames(res) = seq(1,nrow(res))

  return(res)
}
