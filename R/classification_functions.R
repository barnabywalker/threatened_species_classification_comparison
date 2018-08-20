###############################################################################
# Functions used to run the overall classification, invariant of method.      #
###############################################################################

library(here)

source(here("R", "method_functions.R"))
source(here("R", "data_processing_functions.R"))

library(tidyverse)

run_classification <- function(method_info, 
                               report_file=NULL, 
                               save_file=NULL, 
                               seed=NA) {
  #' Runs a classification of threatened species by a particular method.
  #' 
  #' Runs a classification of threatened species based on information
  #' supplied about the classification method.
  #' 
  #' @param method_info A list of information about the classifcation method.
  #' 
  #' @param generate_report Whether to generate a method-specific html report.
  #' 
  #' @param save_info Whether to save intermediate data to file.
  #' 
  #' @param seed An integer to set as the random seed.
  #' 
  #' @return A data frame of the classification results.
  
  if (!is.na(seed)) {
    set.seed(seed)
  }
  
  if (!is.null(save_file)) {
    write_rds(method_info, paste(save_file, ".rds", sep=""))
  }
  
  if (!is.null(save_file)) {
    save_info <- TRUE
  } else {
    save_info <- FALSE
  }
  
  unprocessed_data <- read_csv(method_info$data_path)
  
  processed_data <- preprocess_data(unprocessed_data, 
                                    method_info$method, 
                                    parameters=method_info$preprocess_parameters, 
                                    save_info=save_info)
  
  data_splits <- split_data(processed_data, method_info$split_parameters)
  
  method_results <- run_method(data_splits, parameters=method_info, save_file=save_file)
  
  if (!is.null(report_file)) {
    report_date <- Sys.Date()
    report_file <- here("notebooks", "results_notebooks", "individual_results",
                        paste(report_file, ".nb.html", sep=""))
    report_title <- method_info$method
    report_template <- paste(str_replace_all(str_to_lower(method_info$method), " ", "_"), ".R", sep="")
    
    rmarkdown::render(here("analysis", "report_templates", report_template), output_file=report_file)
  }
  
  method_results
}

preprocess_data <- function(data, method, parameters=NULL, save_info=FALSE) {
  #' Preprocess data for classification.
  #' 
  #' Use the information provided in parameters to carry out preprocessing
  #' steps before classification.
  #' 
  #' @param data A dataframe containing the data to preprocess.
  #' 
  #' @param method The name of the classification method to be used.
  #' 
  #' @param parameters A list containing information about how to preprocess.
  #' 
  #' @param save_info Whether to save any information about the preprocessing.
  #' 
  #' @return A dataframe of the preprocessed data
  
  # create the output variable
  data <-
    data %>%
    mutate(category = factor(category, 
                             levels=c("LC", "NT", "VU", "EN", "CR"), 
                             ordered=TRUE),
           threatened = case_when(category > "NT" ~ "threatened",
                                  TRUE ~ "not_threatened"),
           threatened = factor(threatened))
  
  if (method == "Random forests") {
    data <- clean_random_forest_data(data)
  }
  
  if (is.null(parameters)) {
    return(data)
  }
  
  # store variables not used in any of the methods
  stored_columns <-
    data %>%
    select(species, category, criteria, group, threatened)
  
  predictors <- 
    data %>%
    select(-species, -category, -criteria, -group, -threatened)
  
  if (parameters$standardise) {
    scaler <- preProcess(predictors, method = c("center", "scale"))
    predictors <- predict(scaler, predictors)
  }
  
  if (parameters$remove_nzv) {
    nzv_info <- nearZeroVar(predictors, saveMetrics=TRUE)
    nzv_predictors <- nearZeroVar(predictors)
    
    if (length(nzv_predictors) > 0) {
      predictors <- predictors[, -nzv_predictors]
    }
  }
  
  if (parameters$remove_correlated) {
    correlations <-
      predictors %>%
      select_if(is.double) %>%
      cor()
    
    hi_correlations <- findCorrelation(correlations, cutoff=parameters$correlated_predictors_cutoff, names=T)
    
    predictors <-
      predictors %>%
      select(-one_of(hi_correlations))
  }
  
  processed_data <- cbind(stored_columns, predictors)

  # genus has too many levels for random forests, so needs to be an integer, but not scaled
  if (method == "Random forests") {
    processed_data <- mutate(processed_data, genus = as.integer(genus))
  }
  
  if (parameters$clean_locations) {
    processed_data <- fill_locality(processed_data, 
                                     locality_level=parameters$locality_level)
  }
  
  if (parameters$clean_coordinates) {
    processed_data <- remove_bad_coordinates(processed_data)
  }
  
  if (save_info) {
    if (parameters$standardise) {
      write_rds(scaler, 
                here("output", 
                     paste(str_replace_all(str_to_lower(method), " ", "_"), 
                           "_scaler.rds", sep="")))
    }
    
    if (parameters$remove_nzv) {
      write_csv(nzv_info, 
                here("output", 
                     paste(str_replace_all(str_to_lower(method), " ", "_"), 
                           "_nzv_info.csv", sep="")))
      }
    
    if (parameters$remove_correlated) {
      write_csv(as.data.frame(correlations), 
                here("output", 
                     paste(str_replace_all(str_to_lower(method), " ", "_"), 
                           "_correlated_predictors.csv", sep="")))
    }
  }
  
  return(processed_data)
}

split_data <- function(data, parameters=NULL) {
  #' Split the data into train and test sets.
  #' 
  #' Split the data into different data sets for
  #' model training and validation. If no split ratio is provided,
  #' the train and test set will both be the entirety of the data.
  #' 
  #' @param data A dataframe of the data to be split.
  #' 
  #' @param parameters A list of parameters describing how to split the data.
  #' 
  #' @return A list containing the train and test set data.
  
  if (!is.null(parameters)) {
    train_idx <- createDataPartition(data$threatened, p=parameters$ratio, list=FALSE, times=1)
  
    train_set <- data[train_idx, ]
    test_set <- data[-train_idx, ]
  } else {
    train_set <- data
    test_set <- data
  }
  
  train_info <-
    train_set %>%
    select(species, group, category, criteria)
  test_info <-
    test_set %>%
    select(species, group, category, criteria)
  
  train_set <-
    train_set %>%
    select(-species, -group, -category, -criteria)
  test_set <-
    test_set %>%
    select(-species, -group, -category, -criteria)
  
  list(
    train = train_set,
    test = test_set,
    train_info = train_info,
    test_info = test_info
  )
    
}

run_method <- function(data, parameters, save_file=NULL) {
  #' Run the specified classification method.
  #' 
  #' @param data A list containing a train and test set dataframe.
  #' 
  #' @param parameters A list of parameters describing the method.
  #' 
  #' @param save_info Whether to save any intermediate information about the method.
  #' 
  #' @return A dataframe of the classification results.
  
  if (!is.null(save_file)) {
    save_info <- TRUE
  } else {
    save_info <- FALSE
  }
  
  results <- switch(parameters$method,
                    `Random forests` = run_random_forests(data, parameters, save_info),
                    `US method` = run_us_method(data, parameters, save_info),
                    `rCAT` = run_rcat(data, parameters, save_info),
                    `ConR` = run_conr(data, parameters, save_info),
                    `Specimen count` = run_specimen_count(data, parameters, save_info), 
                    stop(sprintf("Unrecognised method type '%s', please choose one of: 
                                 'Random forests', 'US method', 'rCAT', 'ConR', 'Specimen count'.", 
                                 parameters$method)))
   
  if (!is.null(save_file)) {
    write_csv(results, here("output", paste(save_file, ".csv", sep="")))
  }
  
  results %>%
    ungroup() %>%
    select(species, category, criteria, group, obs, pred) %>%
    mutate(method = parameters$method)
}


