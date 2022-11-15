

# test_json_string <-
#   '{
#       "gender" : "Male" ,
#       "SeniorCitizen" : 0 ,
#       "Partner" : "Yes" ,
#       "Dependents" : "Yes" ,
#       "tenure" : 70  ,
#       "PhoneService" : "Yes" ,
#       "MultipleLines" : "Yes",
#       "InternetService" : "No" ,
#       "OnlineSecurity" : "No internet service" ,
#       "OnlineBackup" :  "No internet service" ,
#       "DeviceProtection": "No internet service" ,
#       "TechSupport" : "No internet service",
#       "StreamingTV" : "No internet service",
#       "StreamingMovies" : "No internet service",
#       "Contract" : "Two year",
#       "PaperlessBilling" : "No",
#       "PaymentMethod" : "Mailed check",
#       "MonthlyCharges" : 25.4,
#       "TotalCharges" : 1782.05
# }'










suppressWarnings(suppressMessages(library(tidyverse)))
suppressWarnings(suppressMessages(library(lubridate)))
suppressWarnings(suppressMessages(library(data.table)))
suppressWarnings(suppressMessages(library(xgboost)))
suppressWarnings(suppressMessages(library(httr)))
suppressWarnings(suppressMessages(library(glue)))


trained_model  <- read_rds('model/artifacts/model.rds')
encode_these   <- trained_model$variables_to_encode
target_class          <- trained_model$target_class
other_class           <- trained_model$other_class
id_column             <- trained_model$id_column
threshold <- 0.50




prediction_scorer <- function(customer) {
  
  
  
  
  ## initialize scores
  score  <- 0
  
  ## accept or reject
  
  encodings_tmp <-
    trained_model$encodings %>%
    map(function(x) {
      if (is.data.frame(x)) {
        x[, 2, drop = TRUE] %>% set_names(x[, 1, drop = TRUE])
      } else {
        x
      }
    })
  for (catvar in encode_these) {
    customer[[catvar]] <-
      encodings_tmp[[catvar]][customer[[catvar]] %>% as.character()]
  }
  
  
  score <- 
    predict(trained_model$mdl, 
            data.matrix(customer %>% select(all_of(trained_model$mdl$feature_names))))
  
  
  score
}




#* @post /predict
function(req) {
# caller = function(req) {
  
  ## grab the request body 'req' and put it into the variable 'customer'
  customer <- jsonlite::fromJSON(req$postBody) %>% as_tibble()
  # customer <- jsonlite::fromJSON(req) %>% as_tibble()
  customer %>% glimpse()
  
  ## placeholder for JSON string to be printed at the end
  result <-
    tibble(prediction_prob = 0,
           prediction_label = '',
           warnings = '')
  
  ## parameters that we need
  necessary_params <- trained_model$mdl$feature_names
  
  ## if we do NOT have all we need...
  if (!all(necessary_params %in% names(customer))) {
    
    
    result$prediction_prob <- 0
    result$prediction_label <- ''
    result$warnings <- 'Some necessary features are missing'
    
  } else {
    
    ## keep only the necessary parameters
    customer <- customer[necessary_params]
    
    ## if any of the necessary parameters are null...
    if (customer %>% sapply(is.null) %>% any()) {
      
      result$prediction_prob <- 0.55
      result$prediction_label <- 'UNCOVERED'
      result$warnings <-
          paste('The following required parameters were NULL:', null_parameters)
      
    } else {
      
      print('all features are here')
      prediction_result <- prediction_scorer(customer)
      print(prediction_result)
      print('prediction_result')
      result$prediction_prob  <- prediction_result %>% round(5)
      result$prediction_label <- if_else(prediction_result >= 0.5 , get("target_class"), get("other_class"))
    }
    
  }
  
  c(result$prediction_prob, 
    result$prediction_label, 
    result$warnings)
}





#* @get /healthz
#* @serializer json list(auto_unbox=TRUE)
endpoint.healthz <- function(req) {
  return("it's working perfectly")
}




