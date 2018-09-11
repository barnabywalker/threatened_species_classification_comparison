###############################################################################
##### Script to analyse and compare results from each method.             #####
###############################################################################

library(here)
library(caret)
library(HDInterval)
library(tidyverse)
library(gtools)
library(magrittr)
library(randomForest)

source(here("R", "analysis_functions.R"))

# load results data -----------------------------------------------------------

results_data <- 
  here("output", "all_results_summary.csv") %>%
  read_csv() %>%
  mutate(obs = factor(obs),
         pred = factor(pred, levels=levels(obs)),
         group = recode(group,
                          madagascar_palms = "MadPalms",
                          myrts = "Myrcia",
                          coffee = "Coffee",
                          legumes = "Legumes",
                          ng_orchids_p = "OrchidsNG"))

us_method_results <- 
  here("output", "us_method_results.csv") %>%
  read_csv() %>%
  mutate(obs = factor(obs),
         pred = factor(pred, levels=levels(obs)))

rf_model <-
  here("output", "random_forests_model.rds") %>%
  read_rds()

rf_training_set <- 
  read_csv(here("output", "random_forests_train_set.csv")) %>%
  mutate(group = recode(group,
                          madagascar_palms = "MadPalms",
                          myrts = "Myrcia",
                          coffee = "Coffee",
                          legumes = "Legumes",
                          ng_orchids_p = "OrchidsNG"))

rf_test_set <-
  read_csv(here("output", "random_forests_test_set.csv")) %>%
  mutate(group = recode(group,
                          madagascar_palms = "MadPalms",
                          myrts = "Myrcia",
                          coffee = "Coffee",
                          legumes = "Legumes",
                          ng_orchids_p = "OrchidsNG"))

rf_test_species <- rf_test_set$species
rf_test_category <- rf_test_set$category
rf_test_group <- rf_test_set$group
rf_test_threat <- rf_test_set$threatened

rf_test_model <-
  rf_test_set %>%
  select(-species, -threatened, -category, -group) %>%
  mutate(order=factor(order, 
                      levels=levels(rf_model$trainingData$order)),
         family=factor(family, 
                       levels=levels(rf_model$trainingData$family)),
         realm_value=factor(realm_value, 
                            levels=levels(rf_model$trainingData$realm_value)))

rl_criteria <-
  here("data", "All plants from IUCN Red List since 2002.xlsx") %>%
  readxl::read_excel()

# summarise test set data -----------------------------------------------------

test_set_summary <-
  results_data %>%
  summarise_categories_by(method)

test_set_summary_by_group <-
  results_data %>%
  summarise_categories_by(method, group)

# count assessments with each criteria
test_set_criteria <- 
  results_data %>%
  summarise_criteria_by(method)

# find proportion with any criteria
criteria_presence <-
  results_data %>%
  mutate(has_criteria = !is.na(criteria)) %>%
  group_by(method, obs) %>%
  summarise(with_criteria = sum(has_criteria) / n()) %>%
  spread(obs, with_criteria)

# count criteria only for threatened species
criteria_count_threatened <-
  results_data %>%
  filter(obs == "threatened") %>%
  summarise_criteria_by(method)

# count assessments with each criteria in each group
criteria_count_by_group <- 
  results_data %>%
  summarise_criteria_by(group, method)

criteria_count_threatened_by_group <- 
  results_data %>%
  filter(obs == "threatened") %>%
  summarise_criteria_by(group, method)

# add a summary of the criteria proportions overall for plants on the Red List
criteria_count_by_group <-
  rl_criteria %>%
  rename(criteria=`Red List criteria`) %>%
  mutate(method="IUCN Red List",
         group=NA_character_,
         species=paste(Genus, Species)) %>%
  summarise_criteria_by(group, method) %>%
  bind_rows(criteria_count_by_group)

# analyse overall results -----------------------------------------------------

results_overall <- 
  results_data %>%
  summarise_results_by(method, positive_case="threatened") %>%
  select(-upper_ci, -lower_ci) %>%
  mutate(n_correct = nobs * accuracy) %>%
  model_accuracy_by(method) %>%
  select(-accuracy_p_value)

# analyse results by group ----------------------------------------------------

results_by_group <-
  results_data %>%
  summarise_results_by(method, group, positive_case="threatened") %>%
  select(-upper_ci, -lower_ci) %>%
  mutate(n_correct = nobs * accuracy) %>%
  model_accuracy_by(method, group) %>%
  select(-accuracy_p_value)

# results by category ---------------------------------------------------------
results_by_category <-
  results_data %>%
  summarise_results_by(method, category, positive_case="threatened") %>%
  select(-upper_ci, -lower_ci) %>%
  mutate(n_correct = nobs * accuracy) %>%
  model_accuracy_by(method, category) %>%
  select(-accuracy_p_value)

# pairwise differences between method results ---------------------------------

# tabulate true positives, etc.
confusion_table <-
  results_data %>%
  calculate_confusion_by(method)

# model method performance metrics
confusion_samples <- 
  results_data %>%
  model_metrics_by(method)

# calculate pairwise differences
method_differences <- map_dfr(unique(confusion_samples$method), 
                              ~pairwise_method_test(confusion_samples, .x))

method_significant_differences <-
  method_differences %>%
  rename(method2=method) %>%
  select(-mean, -ci_hi, -ci_lo) %>% 
  mutate(significance = significance == "significant") %>%
  spread(measure, significance)

confusion_samples_summary <-
  confusion_samples %>%
  gather(measure, sample, -method) %>%
  group_by(method, measure) %>%
  summarise(mu = mean(sample), 
            ci_hi=hdi(sample, 0.95)[2], 
            ci_lo=hdi(sample, 0.95)[1]) %>%
  left_join(results_overall %>% 
              ungroup() %>%
              select(method, significance, accuracy, sensitivity, specificity) %>%
              gather(measure, observed_value, -method, -significance),
            by=c("method", "measure"))

# analyse differences in IUCN criteria ----------------------------------------

criteria_accuracy_samples <-
  results_data %>%
  filter(obs == "threatened") %>%
  mutate(a_assessment = case_when(is.na(criteria) ~ FALSE, TRUE ~ str_detect(criteria, "A")),
         b_assessment = case_when(is.na(criteria) ~ FALSE, TRUE ~ str_detect(criteria, "B")),
         c_assessment = case_when(is.na(criteria) ~ FALSE, TRUE ~ str_detect(criteria, "C")),
         d_assessment = case_when(is.na(criteria) ~ FALSE, TRUE ~ str_detect(criteria, "D")),
         correct = obs == pred) %>%
  select(-criteria, -category, -obs, -pred) %>%
  gather(criteria, has_criteria, -species, -group, -method, -correct) %>%
  mutate(criteria = str_remove(criteria, "_assessment"),
         has_criteria = ifelse(has_criteria, "is_criteria", "not_criteria")) %>%
  group_by(method, criteria, has_criteria) %>%
  summarise(n_correct = sum(correct),
            n_wrong = sum(!correct)) %>%
  group_by(method, criteria, has_criteria) %>%
  mutate(samples = list(tibble(n=1:10000, sample=rbeta(10000, 1+n_correct, 1+n_wrong)))) %>%
  select(-n_correct, -n_wrong) %>%
  unnest() %>%
  spread(has_criteria, sample) %>%
  mutate(difference = is_criteria - not_criteria)

criteria_accuracy <-
  criteria_accuracy_samples %>%
  group_by(method, criteria) %>%
  summarise(mean_difference = mean(difference),
            difference_ci_hi = HDInterval::hdi(difference, 0.95)[2],
            difference_ci_lo = HDInterval::hdi(difference, 0.95)[1],
            significance = case_when(difference_ci_lo < 0 & difference_ci_hi > 0 ~ "not significant",
                                     TRUE ~ "significant"))

# analyse differences in criteria by group ------------------------------------

criteria_accuracy_by_group_samples <-
  results_data %>%
  filter(obs == "threatened") %>%
  mutate(a_assessment = case_when(is.na(criteria) ~ FALSE, TRUE ~ str_detect(criteria, "A")),
         b_assessment = case_when(is.na(criteria) ~ FALSE, TRUE ~ str_detect(criteria, "B")),
         c_assessment = case_when(is.na(criteria) ~ FALSE, TRUE ~ str_detect(criteria, "C")),
         d_assessment = case_when(is.na(criteria) ~ FALSE, TRUE ~ str_detect(criteria, "D")),
         correct = obs == pred) %>%
  select(-criteria, -category, -obs, -pred) %>%
  gather(criteria, has_criteria, -species, -group, -method, -correct) %>%
  mutate(criteria = str_remove(criteria, "_assessment"),
         has_criteria = ifelse(has_criteria, "is_criteria", "not_criteria")) %>%
  group_by(method, group, criteria, has_criteria) %>%
    summarise(n_correct = sum(correct),
              n_wrong = sum(!correct)) %>%
  group_by(method, group, criteria, has_criteria) %>%
  mutate(samples = list(tibble(n=1:10000, sample=rbeta(10000, 1+n_correct, 1+n_wrong)))) %>%
  select(-n_correct, -n_wrong) %>%
  unnest() %>%
  spread(has_criteria, sample) %>%
  mutate(difference = is_criteria - not_criteria)

criteria_accuracy_by_group <-
  criteria_accuracy_by_group_samples %>%
  group_by(method, group, criteria) %>%
  summarise(mean_difference = mean(difference),
            difference_ci_hi = HDInterval::hdi(difference, 0.95)[2],
            difference_ci_lo = HDInterval::hdi(difference, 0.95)[1],
            significance = case_when(difference_ci_lo < 0 & difference_ci_hi > 0 ~ "not significant",
                                     TRUE ~ "significant")) 


# analyse random forests predictor importances ----------------------------------

overall_importance <- 
  importance(rf_model$finalModel, type=1, scale=F) %>%
  as.data.frame() %>%
  rename(mean_decrease_accuracy=MeanDecreaseAccuracy) %>%
  mutate(predictor = rownames(.),
         rank = row_number(desc(mean_decrease_accuracy)))

local_importances <-
  rf_model$finalModel$localImportance %>%
  t() %>%
  as.tibble() %>%
  mutate(group=rf_training_set$group) %>%
  gather(predictor, mean_decrease_accuracy, -group)

importance_by_group <-
  local_importances %>%
  group_by(group, predictor) %>%
  summarise(mean_decrease_accuracy = mean(mean_decrease_accuracy)) %>%
  mutate(rank = row_number(desc(mean_decrease_accuracy)))

top5_importances <-
  importance_by_group %>%
  filter(rank <= 5) %>%
  select(-mean_decrease_accuracy) %>%
  spread(rank, predictor)

top5_importances_values <-
  importance_by_group %>%
  filter(rank <= 5) %>%
  select(-predictor) %>%
  spread(rank, mean_decrease_accuracy)

# analyse output of US method -------------------------------------------------

us_method_steps <- c("temporal1", "spatial", "abundance", "temporal2")

us_method_counts <-
  us_method_results %>%
  mutate(classified_at = case_when(temporal1 == TRUE ~ "temporal1",
                                   spatial == TRUE ~ "spatial",
                                   abundance == TRUE ~ "abundance",
                                   temporal2 == TRUE ~ "temporal2",
                                   TRUE ~ "temporal2")) %>%
  mutate(classified_at = factor(classified_at, levels=us_method_steps, ordered=TRUE)) %>%
  group_by(pred, classified_at) %>%
  summarise(accuracy = sum(pred == obs) / n(),
            n = n()) %>%
  ungroup() %>%
  mutate(pred = as.character(pred)) %>%
  complete(classified_at, fill=list(n=0, pred="possibly_extinct", accuracy=NA))

# write the data into one excel file ------------------------------------------
writexl::write_xlsx(list(test_set_summary = test_set_summary,
                         test_set_summary_by_group = test_set_summary_by_group,
                         test_set_criteria = test_set_criteria,
                         test_set_criteria_by_group = criteria_count_by_group,
                         test_set_has_criteria = criteria_presence,
                         test_set_criteria_threatened = criteria_count_threatened,
                         test_set_criteria_threat_group = criteria_count_threatened_by_group,
                         overall_results = results_overall,
                         results_by_group = results_by_group,
                         results_by_category = results_by_category,
                         confusion_table = confusion_table,
                         confusion_samples_summary = confusion_samples_summary,
                         differences_between_methods = method_differences,
                         pairwise_significance = method_significant_differences,
                         criteria_accuracy_samples = criteria_accuracy_samples,
                         criteria_accuracy = criteria_accuracy,
                         criteria_accuracy_group_samples = criteria_accuracy_by_group_samples,
                         criteria_accuracy_by_group = criteria_accuracy_by_group,
                         predictor_importance = overall_importance,
                         predictor_importance_by_group = importance_by_group,
                         top5_importances = top5_importances,
                         top5_importances_values = top5_importances_values,
                         us_method_steps = us_method_counts),
                    here("output", "method_comparison_and_analysis.xlsx"))
