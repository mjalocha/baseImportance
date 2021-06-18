#' Calculate class for each base variable
#'
#' @param frame - dataframe with columns: model variable name, base variable name, model id, and the rest of the columns concern the impact of the variables
#' @param base_columns - model variable name, base variable name, model id column names
#' @param metric_columns - matric column names
#'
#' @return dataframe with columns: model variable name, base variable name, model id, class, and the rest of the columns concern the impact of the variables
#'
#' @import dplyr
#' @import tidyr

classify_variables <- function(frame, base_columns, metric_columns){

  #Calculate classes for each metric
  for(metric in metric_columns){
    temp = data.frame(frame %>% select(all_of(metric)) %>% unlist() %>% simple_quantiles())
    colnames(temp) = paste0(metric, "_class")
    frame = cbind(frame, temp)
  }

  #For each variable take into account min class
  base_variables = frame$base_variable %>% unique()
  min_classes = data.frame("base_variable" = base_variables, "class" = NA)
  metric_classes = paste0(metric_columns, "_class")

  for(var in base_variables){
    var_frame = frame[frame$base_variable == var,]
    class = apply(var_frame %>% select(all_of(metric_classes)), 1, function(x){min(x)}) %>% min()
    min_classes$class[min_classes$base_variable == var] = class
  }

  res = left_join(frame, min_classes, by = "base_variable")

  return(res %>% select(all_of(base_columns), class, all_of(metric_columns)))
}
