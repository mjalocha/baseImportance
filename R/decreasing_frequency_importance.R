#' Calculate base variables importance for each model
#'
#' @param frame - dataframe with columns: model variable name, base variable name, model id, class, and the rest of the columns concern the impact of the variables
#' @param base_columns - names for first four columns (default: c("model_variable", "base_variable", "model_id", "class"))
#'
#' @return dataframe with columns: variable (base variable), class, metric, decreasing (calculated impact)


decreasing_frequency_importance <- function(frame, base_columns){

  #Assign column names and base variables
  all_cols = colnames(frame)
  base_variables = frame %>%
    select(base_variable) %>%
    unique()

  res = data.frame("variable" = character(), "class" = integer(), "metric" = character(), "decreasing" = numeric())

  #For each base variable calculate decreasing importance
  for(var in base_variables %>% unlist()){
    base_variable_frame = frame[frame[[all_cols[2]]] ==var,]
    class = frame$class[frame[[all_cols[2]]] ==var] %>% .[1]
    for(metric in all_cols[(length(base_columns) + 1):(length(all_cols))]){

      #Find indexes of max values and verify wether there are one or more
      temp_values = base_variable_frame[[metric]]
      max_idx = which(temp_values == max(temp_values))

      if(nrow(base_variable_frame) == 1){
        temp_res = data.frame("variable" = var, "class" = class, "metric" = metric, "decreasing" = temp_values)
      } else{
        if(length(max_idx) > 1){
          calculated_importance = c()
          for(idx in max_idx){
            calculated_importance_decreasing = c(calculated_importance, decreasing_importance(base_imp = base_variable_frame[idx, metric],
                                                                                              rest_imp = base_variable_frame[-idx, metric]))
          }
          max_idx = idx
          calculated_importance_decreasing = max(calculated_importance_decreasing)
        } else{
          calculated_importance_decreasing = decreasing_importance(base_imp = base_variable_frame[max_idx, metric],
                                                                   rest_imp = base_variable_frame[-max_idx, metric])
        }
        temp_res = data.frame("variable" = var, "class" = class, "metric" = metric, "decreasing" = calculated_importance_decreasing %>% unlist())
      }

      res = rbind(res, temp_res)

    }
  }

  return(res)
}
