###############################################################################
# Functions used to run specific methods. Add a new function here for a new   #
# classification method.                                                      #
###############################################################################

library(caret)
library(randomForest)
library(tidyverse)

run_random_forests <- function(data, parameters, save_info=FALSE) {
  #' Run a random forests classification.
  #' 
  #' Random forests classification with a user-specified number of trees,
  #' tuning the number of predictors in each split (mtry) using
  #' user-specified cross-validation by maximising the area under
  #' receiver-operator curve (ROC).
  #' 
  #' @param data A list containing the train and test data, and any info about
  #' each set.
  #' 
  #' @param parameters A list containing the parameters to use for tuning.
  #' 
  #' @param save_info Whether to save the model and test and train sets.
  #' 
  #' @return A data frame of the test set predictions.

  fit_control <- trainControl(method=parameters$tuning_parameters$cv_method, 
                              number=parameters$tuning_parameters$cv_folds, 
                              repeats=parameters$tuning_parameters$cv_repeats, 
                              classProbs=TRUE, summaryFunction=twoClassSummary)
  
  model <- train(x=as.data.frame(select(data$train, -threatened)), y=pull(data$train, threatened), 
                 method="rf", metric="ROC", 
                 trControl=fit_control,
                 importance=TRUE, localImp=TRUE, 
                 verbose=FALSE, 
                 trees=parameters$method_parameters$n_trees,
                 tuneGrid=parameters$method_parameters$hyperparameter_tuning_grid)
  
  results <- 
    predict(model, data$test, type="prob") %>%
    cbind(data$test_info) %>%
    mutate(obs = data$test$threatened,
           pred = predict(model, data$test),
           prob = threatened) %>%
    select(-threatened, -not_threatened)
  
  if (save_info) {
    write_rds(model, here("output", "random_forests_model.rds"))
    
    data$test %>%
      cbind(data$test_info) %>%
      write_csv(here("output", "random_forests_test_set.csv"))
    data$train %>%
      cbind(data$train_info) %>%
      write_csv(here("output", "random_forests_train_set.csv"))
  }
  
  return(results)
}

run_us_method <- function(data, parameters, save_info=FALSE) {
  #' Classify species by the US method.
  #' 
  #' Classify the threat status of a species based on the US method
  #' described by Krupnick et al.
  #' 
  #' @param data A list containing the train and test data, and any info about
  #' each set.
  #' 
  #' @param parameters A list containing any parameters to use in the method.
  #' 
  #' @param save_info Whether to save any intermediate results.
  #' 
  #' @return A data frame of the test set predictions.
  
  median_specimens <-
    data$test %>%
    cbind(data$test_info) %>%
    count(species) %>%
    pull(n) %>%
    median()
  
  median_specimens_after_1960 <- 
    data$test %>%
    cbind(data$test_info) %>%
    filter(year > 1960) %>%
    count(species) %>%
    pull(n) %>%
    median()
  
  results <-
    data$test %>%
    cbind(data$test_info) %>%
    group_by(species) %>%
    summarise(category = first(category),
              group = first(group),
              threatened = first(threatened),
              criteria = first(criteria),
              after_1960 = sum(ifelse(is.na(year), FALSE, year > 1960)),
              temporal1 = all(ifelse(is.na(year), FALSE, year < 1900)),
              spatial = length(unique(locality)) >= 6,
              abundance = n() <= median_specimens,
              temporal2 = after_1960 <= median_specimens_after_1960) %>%
    mutate(obs = threatened,
           pred = case_when(temporal1 ~ "threatened",
                            spatial ~ "not_threatened",
                            abundance ~ "threatened",
                            temporal2 ~ "threatened",
                            TRUE ~ "not_threatened"),
           pred = factor(pred, levels=levels(obs)))
  
  return(results)
}

run_rcat <- function(data, parameters, save_info=FALSE) {
  #' Classify species using rCAT.
  #' 
  #' Classify the threat status of species based on the extent of
  #' occurrence (EOO), as calculated by rCAT.
  #' 
  #' @param data A list containing the train and test data, and any info about
  #' each set.
  #' 
  #' @param parameters A list containing any parameters to use in the method.
  #' 
  #' @param save_info Whether to save intermediate info.
  #' 
  #' @return A data frame of the test set predictions.
  
  extra_info <-
    data$test_info %>%
    mutate(threatened = data$test$threatened) %>%
    group_by(species) %>%
    summarise(group = first(group),
              category = first(category),
              criteria = first(criteria),
              threatened = first(threatened))
  
  results <-
    data$test %>%
    select(-threatened) %>%
    mutate(species = data$test_info$species) %>%
    assess_species_rcat(cellsize=2000) %>%
    inner_join(extra_info, by="species") %>%
    mutate(obs = threatened,
           pred = case_when(eoo %in% c("VU", "EN", "CR") ~ "threatened",
                            TRUE ~ "not_threatened"),
           pred = factor(pred, levels=levels(obs)))
  
  return(results)
}

run_conr <- function(data, parameters, save_info=FALSE) {
  #' Classify species using ConR.
  #' 
  #' Classify the threat status of species based on preliminary
  #' assessments generated by ConR
  #' 
  #' @param data A list containing the train and test data, and any info about
  #' each set.
  #' 
  #' @param parameters A list containing any parameters to use in the method.
  #' 
  #' @param save_info Whether to save intermediate info.
  #' 
  #' @return A data frame of the test set predictions.
  
  results <-
    data$test %>%
    cbind(data$test_info) %>%
    group_by(species, category, criteria, threatened, group) %>%
    nest() %>%
    mutate(nobs = map_int(data, nrow)) %>%
    arrange(nobs) %>%
    mutate(assessment = map(data, assess_species_conr)) %>%
    select(-data) %>%
    unnest() %>% 
    filter(!is.na(pred_category)) %>%
    mutate(obs = threatened,
           pred = case_when(pred_category %in% c("VU", "EN", "CR") ~ "threatened",
                            TRUE ~ "not_threatened"),
           pred = factor(pred, levels=levels(obs))) %>%
    select(-threatened)
  
  return(results)
}

run_specimen_count <- function(data, parameters, save_info=FALSE) {
  #' Classify species using a threshold number of specimens.
  #' 
  #' Classify the threat status of species based on whether or not the number
  #' of specimens is over a threshold value. This threshold is chosen by
  #' calculating the accuracy for a threshold from 1 to 100, and taking the
  #' threshold with the highest accuracy.
  #' 
  #' @param data A list containing the train and test data, and any info about
  #' each set.
  #' 
  #' @param parameters A list containing any parameters to use in the method.
  #' 
  #' @param save_info Whether to save the intermediate info.
  #' 
  #' @return A data frame of the test set predictions.
  
  threshold_measure <- rlang::sym(parameters$threshold_measure)
  
  specimen_counts <-
    data$test %>%
    cbind(data$test_info) %>%
    group_by(species, group, threatened, category, criteria) %>%
    summarise(n_specimens = n()) %>%
    mutate(obs = threatened)
  
  threshold_accuracy <- map_dfr(seq(1:100), calculate_threshold_accuracy, specimen_counts)
  
  best_threshold <-
    threshold_accuracy %>%
    arrange(desc(!! threshold_measure)) %>%
    head(1) %>%
    pull(threshold)
  
  results <- 
    specimen_counts %>%
    mutate(pred = case_when(n_specimens >= best_threshold ~ "not_threatened",
                            TRUE ~ "threatened"),
           threshold = best_threshold)
  
  if (save_info) {
    write_csv(threshold_accuracy, here("output", "specimen_count_threshold_accuracy.csv"))
  }
  
  return(results)
}

calculate_threshold_accuracy <- function(threshold, specimens, positive_case="threatened") {
  #' Calculate the accuracy for a given threshold number of specimens.
  #' 
  #' @param threshold An integer value for the threshold number of specimens
  #' 
  #' @param specimens A dataframe with the number of specimens for each species,
  #' and the observed threat status.
  #' 
  #' @return A single-row data frame of the accuracy and other 
  #' performance statistics.
  
  predictions <- 
    case_when(specimens$n_specimens >= threshold ~ "not_threatened",
              TRUE ~ "threatened") %>%
    factor(levels=levels(specimens$obs))
  
  confusion <- confusionMatrix(predictions, specimens$obs, mode="everything", positive=positive_case)
  
  tibble(threshold=threshold, 
         accuracy=confusion$overall["Accuracy"],
         default_accuracy=confusion$overall["AccuracyNull"],
         accuracy_p_value=confusion$overall["AccuracyPValue"],
         sensitivity=confusion$byClass["Sensitivity"],
         specificity=confusion$byClass["Specificity"],
         precision=confusion$byClass["Precision"],
         recall=confusion$byClass["Recall"])
}

assess_species_rcat <- function(species_df, cellsize) {
  #' Generate conservation assessments for species using rCAT.
  #' 
  #' Calculates the EOO, and AOO for each of a species in a data frame
  #' and generates conservation assessments based on these, using the
  #' rCAT package.
  #' 
  #' @param species_df Data frame of specimens for one or more species.
  #' 
  #' @param cellsize The cell size, in metres, to use for the AOO
  #' calculations.
  #' 
  #' @return A data frame of species with their EOO, AOO, and
  #' conservation assessments.
  
  species_list <- unique(species_df$species)
  results_df <- data.frame(species=character(), 
                           occurrences=integer(),
                           eoo=character(), 
                           eoo_area=double(), 
                           aoo=character(), 
                           aoo_area=double(),
                           stringsAsFactors = FALSE)
  
  for (species in species_list) {
    species_coordinates <- (species_df[species_df$species==species, 
                                       c("latitude", "longitude")])
    colnames(species_coordinates) <- c("lat", "long")
    species_xy <- rCAT::simProjWiz(species_coordinates, rCAT::trueCOGll(species_coordinates))
    
    occurences <- nrow(species_xy)
    eoo_area <- rCAT::EOOarea(species_xy) / -1000000
    aoo_area <- rCAT::AOOsimp(species_xy, cellsize) * (cellsize / 1000)^2
    eoo_rating <- rCAT::EOORating(eoo_area)
    aoo_rating <- rCAT::AOORating(aoo_area)
    
    results_df[nrow(results_df)+1, ] <- list(species, occurences, eoo_rating,
                                             as.numeric(eoo_area), 
                                             aoo_rating, as.numeric(aoo_area))
  }
  results_df$occurrences <- as.numeric(results_df$occurrences)
  return(results_df)
}

assess_species_conr <- function(data) {
  #' Generate conservation assessments for species using rCAT.
  #' 
  #' Calculates the EOO, and AOO for each of a species in a data frame
  #' and generates conservation assessments based on these, using the
  #' rCAT package.
  #' 
  #' @param species_df Data frame of specimens for one or more species.
  #' 
  #' @param cellsize The cell size, in metres, to use for the AOO
  #' calculations.
  #' 
  #' @return A data frame of species with their EOO, AOO, and
  #' conservation assessments.
  
  # CAUTION! This is to stop it failing on things with very large ranges,
  # but will also mask any other errors.
  tryCatch({
    data %>%
      dplyr::select(latitude, longitude) %>%
      as.data.frame() %>%
      mutate(species = "sp") %>%
      ConR::IUCN.eval(DrawMap=FALSE, write_results=FALSE, verbose=FALSE) %>%
      mutate(species = rownames(.)) %>%
      as.tibble() %>%
      rename(pred_category = Category_CriteriaB,
             n_obs = Nbe_unique_occ.,
             n_subpop = Nbe_subPop,
             n_loc = Nbe_loc,
             eoo_area = EOO,
             aoo_area = AOO,
             pred_criteria = Category_code,
             eoo_category = Category_EOO,
             aoo_category = Category_AOO) %>%
      mutate(n_loc = as.double(n_loc)) %>%
      select(-species)
  },
  error=function(cond) {
    tibble(n_obs=NA, 
           n_subpop=NA, 
           n_loc=NA, 
           pred_category=NA, 
           eoo_area=NA, 
           aoo_area=NA, 
           pred_criteria=NA, 
           eoo_category=NA, 
           aoo_category=NA)
  })
}