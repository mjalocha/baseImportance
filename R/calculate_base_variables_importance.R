#' Calculate base variables importance for each model
#'
#' @param importance_frame - dataframe with columns: model variable name, base variable name, model id, class, and the rest of the columns concern the impact of the variables
#' @param divide_by_number - boolean, do you want to divide impact by number of base variables
#' @param weights - weights for importance of variables in each model (default: NULL, equal)
#' @param base_columns - names for first four columns (default: c("model_variable", "base_variable", "model_id", "class"))
#'
#' @return dataframe with columns: variable (base variable), impact (calculated based on several models specified in mdoel_ids), model_id
#' @import dplyr
#' @import tidyr


calculate_base_variables_importance <- function(importance_frame, divide_by_number, weights = NULL, base_columns = c("model_variable", "base_variable", "model_id", "class"))
{

  #Set colnames
  colnames(importance_frame)[1:length(base_columns)] <- base_columns

  #Change metrics type to numeric
  metric_cols = colnames(importance_frame)[colnames(importance_frame) %!in% base_columns]
  importance_frame[,metric_cols] <- sapply(importance_frame[,metric_cols],as.numeric)

  #Join frames
  grouped_variables = importance_frame %>% group_by(.[[1]]) %>% count()
  colnames(grouped_variables) = c("model_variable", "number_of_variables")

  joined_frames = left_join(importance_frame, grouped_variables)

  #Calculate impact for all model id's
  models = importance_frame$model_id %>% unique()

  res = list()
  for(model in models){
    temp_frame <- decreasing_frequency_importance(frame = joined_frames[joined_frames$model_id == model,], divide_by_number = divide_by_number, base_columns = base_columns)
    temp_frame <- calculate_impact(base_variables_importance = temp_frame, metric_cols = metric_cols, weights = weights)
    temp_frame$model_id = model
    res[[paste(model)]] = temp_frame %>% select(variable, impact, model_id)
  }

  return(do.call(rbind.data.frame, res))
}
