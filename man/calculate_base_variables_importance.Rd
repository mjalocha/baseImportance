% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calculate_base_variables_importance.R
\name{calculate_base_variables_importance}
\alias{calculate_base_variables_importance}
\title{Calculate base variables importance for each model}
\usage{
calculate_base_variables_importance(
  importance_frame,
  weights = NULL,
  base_columns = c("model_variable", "base_variable", "model_id")
)
}
\arguments{
\item{importance_frame}{- dataframe with columns: model variable name, base variable name, model id, class, and the rest of the columns concern the impact of the variables}

\item{weights}{- weights for importance of variables in each model (default: NULL, equal)}

\item{base_columns}{- names for first four columns (default: c("model_variable", "base_variable", "model_id"))}

\item{divide_by_number}{- boolean, do you want to divide impact by number of base variables}
}
\value{
dataframe with columns: variable (base variable), impact (calculated based on several models specified in mdoel_ids), model_id
}
\description{
Calculate base variables importance for each model
}
