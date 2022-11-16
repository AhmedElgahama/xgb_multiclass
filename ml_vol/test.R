#!/usr/bin/env Rscript

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
options(dplyr.summarise.inform = FALSE)

source('algorithm/0.common_funcs.R')




# schema <- glue('inputs/data_config/',list.files(path="inputs/data_config"))

data   <- glue('inputs/data/testing/binaryClassificationBaseMainInput/',list.files(path="inputs/data/testing/binaryClassificationBaseMainInput"))



tester <- function(data_path)
{
  ## read jsonfile
  
  # file <- fromJSON(file = schema_path)
  # 
  # id           <-
  #   file$inputDatasets$binaryClassificationBaseMainInput$idField
  # target       <-
  #   file$inputDatasets$binaryClassificationBaseMainInput$targetField
  # target_class <-
  #   file$inputDatasets$binaryClassificationBaseMainInput$targetClass
  # features = file$inputDatasets$binaryClassificationBaseMainInput$predictorFields
  # 
  # 
  # variables_categorical <- c()
  # 
  # for (field in features)
  # {
  #   type <- field[['dataType']]
  #   name <- field[['fieldName']]
  #   
  #   if (type == 'CATEGORICAL')
  #   {
  #     variables_categorical <- c(variables_categorical, name)
  #   } 
  # 
  # }
  # 
  # 
  
  
  trained_model         <- read_rds('model/artifacts/model.rds')
  variables_to_encode   <- trained_model$variables_to_encode
  target_class          <- trained_model$target_class
  other_class           <- trained_model$other_class
  id_column             <- trained_model$id_column
  test_data             <- read_csv(data_path)
  
  # categories <- test_data[target] %>% distinct()
  # categories <- categories[[1]]
  # other_class <- categories[!(categories %in% target_class)]
  
  id <- test_data %>% select(id_column)
  
  test_data %>% head(1) %>% view()
  
  df_test_acc_rej <-
    test_data 
  # %>% mutate(label = test_data %>% select(get('target')))
  
  # my_map2 <- c(0, 1)
  # names(my_map2) <- c(get("other_class"), get("target_class"))
  
  # df_test_acc_rej   <-
    # df_test_acc_rej   %>% mutate(y = my_map2[unlist(label)] %>% as.logical())
  
  if (!is.null(trained_model$encodings)) {
    encodings_tmp <-
      trained_model$encodings %>%
      map(function(x) {
        if (is.data.frame(x)) {
          x[, 2, drop = TRUE] %>% set_names(x[, 1, drop = TRUE])
        } else {
          x
        }
      })
    for (catvar in variables_to_encode) {
      df_test_acc_rej[[catvar]] <-
        encodings_tmp[[catvar]][df_test_acc_rej[[catvar]] %>% as.character()]
    }
  }
  
  if ('mdl' %in% names(trained_model)) {
    model <- trained_model$mdl
  } else {
    model <- trained_model
  }
  
  df_test_acc_rej   <-
    df_test_acc_rej   %>% #select(-y) %>% 
    filter_if(is.numeric, ~ !is.infinite(.))
  
  test_results <-
    tester_func(
      mdl = model,
      test_set = df_test_acc_rej#,
      # target_variable = 'label',
      # target_variable_mapping = my_map2
    )
  
  
  test_results <- test_results$test_predictions
  print(test_results)
  
  tmp <-
    test_results %>%
    mutate(pred = if_else(pred > 0.5, get("target_class"), get("other_class"))) 
  # %>%
  #   mutate(label = df_test_acc_rej$label) %>%
  #   count(label, pred) %>%
  #   pivot_wider(
  #     names_from = pred,
  #     values_from = n,
  #     values_fill = 0
  #   )
  # 
  # print(tmp)
  
  # tmp$TOTAL <- tmp %>% select_if(is.numeric) %>% rowSums()
  
  # suppressMessages({
  #   my_auc <-
  #     auc(roc(
  #       test_results %>% drop_na() %>% pull(actual),
  #       test_results %>% drop_na() %>% pull(pred)
  #     )) %>% as.numeric()
  # })
  # print(glue('AUC (VAL)  = {model$best_score %>% round(3)}'))
  # print(glue('AUC (TEST) = {my_auc %>% round(3)}'))
  # print(glue(''))
  # 
  
  predictions <- cbind(id,tmp)
  
  predictions %>% write.csv('outputs/testing_outputs/test_predictions.csv')
  
}



tester(data)