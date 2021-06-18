#' Calculate base variables importance for combined model (function for users)
#'
#' @param connection - connection to database
#' @param impact - data frame with columns: variable name, impact, class, model_id (baseImpact_combined ora baseImpact result)
#'
#' @export


push_impact_class <- function(connection, impact, table_name = "CM_DAACC.DM_ANL_LIST_OF_MODELS_VARIABLES"){

  for(i in 1:nrow(impact)){
    imp = impact$impact[i]
    class = impact$class[i]
    model = impact$model_id[i]
    variable = impact$variable[i]

    query = paste0("UPDATE ", table_name ," SET FEATURE_IMP = ", imp ,", FEATURE_CLASS = ", class ," WHERE MODEL_ID = '", model ,"' AND VARIABLE_NAME LIKE '", variable ,"'")

    if(!is.na(imp)){
      dbSendUpdate(connection, query)
    }
  }

}
