###############################################################################
# Functions to help compare and analyse the results of each method            #
###############################################################################

library(tidyverse)

pairwise_method_test <- function(data, method_name) {
  #' Calculate pairwise difference in performance of methods.
  #' 
  #' @param data A data frame of method performance data.
  #' 
  #' @param method_name The name of the method to test against.
  #' 
  #' @return A summary of the differences in performance as a data frame.
  
  method_data <- 
    data %>%
    filter(method == method_name) %>%
    gather(measure, value, -method)
  
  other_data <- 
    data %>%
    filter(method != method_name) %>%
    gather(measure, value, -method)
      
  other_data %>%
    group_by(method) %>%
    mutate(difference = value - method_data$value) %>%
    group_by(method, measure) %>%
    summarise(mean=mean(difference),
              ci_hi=quantile(difference, 0.975),
              ci_lo=quantile(difference, 0.025)) %>%
    mutate(significance=case_when(ci_hi < 0 | ci_lo > 0 ~ "significant",
                                  TRUE ~ "not significant"),
           method1=method_name)
}


summarise_results_by <- function(data, ..., positive_case=NULL) {
  #' Summarises the results of a classification by a grouping variable.
  #' 
  #' Pulls statistics out of caret confusionMatrix function.
  #' 
  #' @param summary_var grouping variable.
  #' 
  #' @param data data frame containing classification results.
  #' 
  #' @return summary of results.
  
  summary_vars <- quos(...)
  
  data %>%
    group_by(!!! summary_vars) %>%
    summarise(nobs = n(),
              accuracy = confusionMatrix(pred, obs, positive=positive_case)$overall["Accuracy"],
              default_accuracy = confusionMatrix(pred, obs, positive=positive_case)$overall["AccuracyNull"],
              upper_ci = confusionMatrix(pred, obs, positive=positive_case)$overall["AccuracyUpper"],
              lower_ci = confusionMatrix(pred, obs, positive=positive_case)$overall["AccuracyLower"],
              accuracy_p_value = confusionMatrix(pred, obs, positive=positive_case)$overall["AccuracyPValue"],
              sensitivity = confusionMatrix(pred, obs, positive=positive_case)$byClass["Sensitivity"],
              specificity = confusionMatrix(pred, obs, positive=positive_case)$byClass["Specificity"]) %>%
    ungroup()
}

calculate_confusion_by <- function(data, ..., positive_case="threatened") {
  #' Tabulates a confusion matrix grouped by specified variables.
  #' 
  #' @param data data frame containing classification results.
  #' 
  #' @param ... grouping variables.
  #' 
  #' @param positive_case which level of outcome to use as the positive case.
  #' 
  #' @return A data frame of false positives, etc. tabulated by group.
  group_vars <- quos(...)
  
  negative_case <- levels(data$obs)[levels(data$obs) != positive_case]
  
  data %>%
    group_by(!!! group_vars) %>%
    summarise(tn = sum(obs == pred & pred == negative_case),
              fn = sum(obs != pred & pred == negative_case),
              tp = sum(obs == pred & pred == positive_case),
              fp = sum(obs != pred & pred == positive_case)) %>%
    ungroup()
}

model_metrics_by <- function(data, ..., n_samples=10000) {
  #' Models classification metrics by specified variables, to get
  #' distribution of values.
  #' 
  #' @param data data frame containing classification results.
  #' 
  #' @param ... grouping variables.
  #' 
  #' @param n_samples number of samples to draw.
  #' 
  #' @return Data frame of samples for metrics.
  
  group_vars <- quos(...)
  
  data %>%
    calculate_confusion_by(!!! group_vars) %>%
    group_by(!!! group_vars) %>%
    mutate(samples = list(sample_metrics(tp, fp, tn, fn, n_samples=n_samples))) %>%
    select(-tp, -fp, -tn, -fn) %>%
    unnest() %>%
    mutate(sensitivity = tp / (tp + fn), 
           specificity = tn / (tn + fp), 
           accuracy = (tp + tn) / (tp + fp + fn + tn)) %>%
    select(-tp, -fp, -tn, -fn) %>%
    ungroup()
}

sample_metrics <- function(tp, fp, tn, fn, n_samples=10000) {
  priors <- c(1,1,1,1)
  
  rdirichlet(n_samples, priors + c(tp, fp, tn, fn)) %>%
    as.tibble() %>%
    set_colnames(c("tp", "fp", "tn", "fn"))
}


model_accuracy_by <- function(data, ..., n_samples=10000) {
  #' Models classification accuracy by specified variables, to get
  #' distribution of values.
  #' 
  #' @param data data frame containing classification results.
  #' 
  #' @param ... grouping variables.
  #' 
  #' @param n_samples number of samples to draw.
  #' 
  #' @return Data frame of samples for accuracy.
  #' 
  group_vars <- quos(...)
  
  data %>%
    group_by(!!! group_vars) %>%
    mutate(samples = list(rbeta(n_samples, 1+n_correct, 1+nobs-n_correct))) %>%
    mutate(posterior_accuracy = map_dbl(samples, mean),
           ci_hi = map_dbl(samples, ~hdi(.x, 0.95)[2]),
           ci_lo = map_dbl(samples, ~hdi(.x, 0.95)[1]),
           significance = case_when(ci_lo > default_accuracy ~ "significant",
                                    TRUE ~ "not significant")) %>%
    select(-samples) %>%
    ungroup()
    
}

summarise_categories_by <- function(data, ...) {
  #' Tabulate IUCN Red List categorys by specified variables.
  #' 
  #' @param data data frame of species information with a
  #' column for accuracy.
  #' 
  #' @param ... grouping variables.
  #' 
  #' @return Data frame of tabulated categories.
  group_vars <- quos(...)
  
  data %>%
    count(!!! group_vars, category) %>%
    group_by(!!! group_vars) %>%
    mutate(n_obs = sum(n),
           p = n / n_obs,
           category = paste("p", category, sep="_")) %>%
    select(-n) %>%
    spread(category, p) %>%
    mutate(p_threatened = p_CR + p_EN + p_VU) %>%
    ungroup()
}


summarise_criteria_by <- function(data, ...) {
  #' Tabulate IUCN Red List assessmment criteria by specified variables.
  #' 
  #' @param data data frame of species information with a
  #' column for accuracy.
  #' 
  #' @param ... grouping variables.
  #' 
  #' @return Data frame of tabulated criteria.
  group_vars <- quos(...)
  
  data %>%
    add_count(!!! group_vars) %>%
    mutate(criteria_a = ifelse(is.na(criteria), FALSE, str_detect(criteria, "A")),
           criteria_b = ifelse(is.na(criteria), FALSE, str_detect(criteria, "B")),
           criteria_c = ifelse(is.na(criteria), FALSE, str_detect(criteria, "C")),
           criteria_d = ifelse(is.na(criteria), FALSE, str_detect(criteria, "D")),
           criteria_e = ifelse(is.na(criteria), FALSE, str_detect(criteria, "E")),
           missing = is.na(criteria)) %>%
    select(species, !!! group_vars, starts_with("criteria_"), missing, n) %>% 
    gather(criteria, value, starts_with("criteria_"), missing) %>%
    mutate(criteria = str_extract(criteria, "(?<=\\_)\\w")) %>%
    group_by(!!! group_vars, criteria) %>%
    summarise(p = sum(value) / first(n)) %>%
    mutate(criteria = ifelse(is.na(criteria), "missing", criteria)) %>%
    spread(criteria, p) %>%
    arrange(!!! group_vars) %>%
    ungroup()
}


