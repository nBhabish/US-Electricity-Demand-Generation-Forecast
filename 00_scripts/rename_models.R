rename_models <- function(data){
  
  data <- data %>% 
    mutate(.model_desc = case_when(.model_desc == "PROPHET W/ REGRESSORS - Tuned" ~ "PROPHET - Tuned",
                                   .model_desc == "PROPHET W/ REGRESSORS" ~ "PROPHET",
                                   .model_desc == "PROPHET W/ XGBOOST ERRORS" ~ "PROPHET W/ XGBOOST", 
                                   .model_desc == "ENSEMBLE (MEAN): 2 MODELS" ~ "ENSEMBLE",
                                   TRUE ~ .model_desc))
  
  return(data)
}

