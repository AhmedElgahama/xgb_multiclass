#!/usr/bin/env /usr/bin/Rscript

## ---- INIT ----

library(ggplot2)
library(tibble)
library(tidyr)
library(readr)
library(purrr)
library(dplyr)
library(stringr)
library(lubridate)
library(plotly)
library(glue)
library(zeallot)
library(xgboost)
library(pROC)
library(forcats)
library(rjson)
library(caTools)
library(imputeTS)
options(dplyr.summarise.inform = FALSE)

source('algorithm/0.common_funcs.R')


## read jsonfile


schema <- glue('inputs/data_config/',list.files(path="inputs/data_config"))

data   <- glue('inputs/data/training/binaryClassificationBaseMainInput/',list.files(path="inputs/data/training/binaryClassificationBaseMainInput"))


trainer <- function(schema_path,data_path)
{
  
  
  file <- fromJSON(file = schema_path)
  
  id_column    <-
    file$inputDatasets$binaryClassificationBaseMainInput$idField
  target_column       <-
    file$inputDatasets$binaryClassificationBaseMainInput$targetField
  target_class <-
    file$inputDatasets$binaryClassificationBaseMainInput$targetClass
  features = file$inputDatasets$binaryClassificationBaseMainInput$predictorFields
  
  
  exp_vars            <- c()
  variables_to_encode <- c()
  variables_numeric     <- c()
  
  for (field in features)
  {
    type <- field[['dataType']]
    name <- field[['fieldName']]
    
    if (type == 'CATEGORICAL')
    {
      variables_to_encode <- c(variables_to_encode, name)
      exp_vars           <- c(exp_vars, name)
    } else
    {
      exp_vars           <- c(exp_vars, name)
      variables_numeric    <- c(variables_numeric, name)
    }
  }
  
  
  ## ---- LOAD DATA ----
  
  full_data <- read_csv(data_path) %>% drop_na(id_column,target_column)
  
  full_data[variables_to_encode] <- sapply(full_data[variables_to_encode],as.character)
  full_data[variables_numeric]   <- sapply(full_data[variables_numeric],as.numeric)
  
  ## impute missing values

  id     <- full_data[,id_column]
  target <- full_data[,target_column]  
  
  full_data_numeric <- full_data %>% select(variables_numeric) %>% na_mean(option = "mean")
  full_data_categorical <- full_data  %>% select(variables_to_encode) %>% 
    mutate(across(everything(), ~replace_na(.x, calc_mode(.x))))
  
   
  full_data <- cbind(id,full_data_numeric,full_data_categorical,target)
  
  
  set.seed(6789)
  split = sample.split(full_data[[target_column]], SplitRatio = 0.7)
  df_train_split = subset(full_data, split == TRUE)
  df_val_split = subset(full_data, split == FALSE)
  
  ## train-val-test split
  categories <- full_data[target_column] %>% distinct()
  categories <- categories[[1]]
  other_class <- categories[!(categories %in% target_class)]
  
  
  df_train <-
    df_train_split %>% mutate(label = df_train_split %>% select(get('target_column'))) 
  df_val   <-
    df_val_split   %>% mutate(label = df_val_split %>% select(get('target_column'))) 
  
  my_map <- c(0, 1)
  names(my_map) <- c(get("other_class"), get("target_class"))
  
  df_train <-
    df_train %>% mutate(y = my_map[unlist(label)] %>% as.logical())
  df_val   <-
    df_val   %>% mutate(y = my_map[unlist(label)] %>% as.logical())

  c(df_train, df_val, dummy, encodings) %<-% target_encode_these(df_train, df_val, variables_to_encode)
  df_train <-
    df_train %>% select(-y) %>% filter_if(is.numeric, ~ !is.infinite(.))
  df_val   <-
    df_val   %>% select(-y) %>% filter_if(is.numeric, ~ !is.infinite(.))
  
  colnames(df_train) <-
    colnames(df_train) %>% str_replace('_target_encoded$', '')
  colnames(df_val) <-
    colnames(df_val) %>% str_replace('_target_encoded$', '')
  
  trained_model <-
    trainer_func(
      train_set      = df_train,
      validation_set = df_val,
      explanatory_variables = exp_vars ,
      target_variable = 'label',
      target_variable_mapping = my_map
    )
  
  
  
  trained_model$exp_vars <- exp_vars
  trained_model$target_class <- target_class
  trained_model$other_class <- other_class
  trained_model$id_column <- id_column
  trained_model$variables_to_encode <- variables_to_encode
  trained_model$encodings <- encodings
  trained_model$best_params %>% as.data.frame() %>% print()
  trained_model %>% write_rds('model/artifacts/model.rds')
  
}


trainer(schema,data)
