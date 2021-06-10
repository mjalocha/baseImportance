#' Sum up impacts of the variable including weights
#'
#' @param base_variables_importance - dataframe with calculated base variables importance
#' @param metric_cols - column names with metrics
#' @param weights - weights for importance of variables in each model (default: NULL, equal)
#'
#' @return temp_frame - dataframe with the combined impact of the variables

calculate_impact <- function(base_variables_importance, metric_cols, weights = NULL){

  #Verify whether the user has entered the weights
  if(is.null(weights)){
    weights = rep(1,length(metric_cols))/length(metric_cols)
  }

  #Calculate impact based on weights
  temp_frame = spread(base_variables_importance,key = metric,value = decreasing)

  for(metric in 1:length(metric_cols)){
    temp_frame[[metric_cols[metric]]] = min_max_norm(temp_frame[[metric_cols[metric]]])

    if(metric == 1){
      impact_col = temp_frame[[metric_cols[metric]]] * weights[metric]
    } else{
      impact_col = impact_col + temp_frame[[metric_cols[metric]]] * weights[metric]
    }

  }
  temp_frame$impact = impact_col

  return(temp_frame)
}
