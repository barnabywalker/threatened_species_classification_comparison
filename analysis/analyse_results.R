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
  count(method, category) %>%
  group_by(method) %>%
  mutate(n_obs = sum(n),
         p = n / n_obs,
         category = paste("p", category, sep="_")) %>%
  select(-n) %>%
  spread(category, p) %>%
  mutate(p_threatened = p_CR + p_EN + p_VU)

test_set_summary_by_group <-
  results_data %>%
  count(method, group, category) %>%
  group_by(method, group) %>%
  mutate(n_obs = sum(n),
         p = n / n_obs,
         category = paste("p", category, sep="_")) %>%
  select(-n) %>%
  spread(category, p) %>%
  mutate(p_threatened = p_CR + p_EN + p_VU)

# count assessments with each criteria
test_set_criteria <- 
  results_data %>%
  add_count(method) %>%
  mutate(criteria_a = ifelse(is.na(criteria), FALSE, str_detect(criteria, "A")),
         criteria_b = ifelse(is.na(criteria), FALSE, str_detect(criteria, "B")),
         criteria_c = ifelse(is.na(criteria), FALSE, str_detect(criteria, "C")),
         criteria_d = ifelse(is.na(criteria), FALSE, str_detect(criteria, "D")),
         criteria_e = ifelse(is.na(criteria), FALSE, str_detect(criteria, "E")),
         missing = is.na(criteria)) %>%
  select(-category, -criteria, -obs, -pred, -group) %>%
  gather(criteria, value, -species, -method, -n) %>%
  mutate(criteria = str_extract(criteria, "(?<=\\_)\\w")) %>%
  group_by(method, criteria) %>%
  summarise(p = sum(value) / first(n)) %>%
  mutate(criteria = ifelse(is.na(criteria), "missing", criteria)) %>%
  spread(criteria, p) %>%
  arrange(method)

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
  add_count(method) %>%
  mutate(criteria_a = ifelse(is.na(criteria), FALSE, str_detect(criteria, "A")),
         criteria_b = ifelse(is.na(criteria), FALSE, str_detect(criteria, "B")),
         criteria_c = ifelse(is.na(criteria), FALSE, str_detect(criteria, "C")),
         criteria_d = ifelse(is.na(criteria), FALSE, str_detect(criteria, "D")),
         criteria_e = ifelse(is.na(criteria), FALSE, str_detect(criteria, "E")),
         missing = is.na(criteria)) %>%
  select(-category, -criteria, -obs, -pred, -group) %>%
  gather(criteria, value, -species, -method, -n) %>%
  mutate(criteria = str_extract(criteria, "(?<=\\_)\\w")) %>%
  group_by(method, criteria) %>%
  summarise(p = sum(value) / first(n)) %>%
  mutate(criteria = ifelse(is.na(criteria), "missing", criteria)) %>%
  spread(criteria, p) %>%
  arrange(method)

# count assessments with each criteria in each group
criteria_count_by_group <- 
  results_data %>%
  add_count(group, method) %>%
  mutate(criteria_a = ifelse(is.na(criteria), FALSE, str_detect(criteria, "A")),
         criteria_b = ifelse(is.na(criteria), FALSE, str_detect(criteria, "B")),
         criteria_c = ifelse(is.na(criteria), FALSE, str_detect(criteria, "C")),
         criteria_d = ifelse(is.na(criteria), FALSE, str_detect(criteria, "D")),
         criteria_e = ifelse(is.na(criteria), FALSE, str_detect(criteria, "E")),
         missing = is.na(criteria)) %>%
  select(-category, -criteria, -obs, -pred) %>%
  gather(criteria, value, -species, -group, -method, -n) %>%
  mutate(criteria = str_extract(criteria, "(?<=\\_)\\w")) %>%
  group_by(method, group, criteria) %>%
  summarise(p = sum(value) / first(n)) %>%
  mutate(criteria = ifelse(is.na(criteria), "missing", criteria)) %>%
  filter(!is.na(group)) %>%
  spread(criteria, p) %>%
  arrange(group)

criteria_count_threatened_by_group <- 
  results_data %>%
  filter(obs == "threatened") %>%
  add_count(group, method) %>%
  mutate(criteria_a = ifelse(is.na(criteria), FALSE, str_detect(criteria, "A")),
         criteria_b = ifelse(is.na(criteria), FALSE, str_detect(criteria, "B")),
         criteria_c = ifelse(is.na(criteria), FALSE, str_detect(criteria, "C")),
         criteria_d = ifelse(is.na(criteria), FALSE, str_detect(criteria, "D")),
         criteria_e = ifelse(is.na(criteria), FALSE, str_detect(criteria, "E")),
         missing = is.na(criteria)) %>%
  select(-category, -criteria, -obs, -pred) %>%
  gather(criteria, value, -species, -group, -method, -n) %>%
  mutate(criteria = str_extract(criteria, "(?<=\\_)\\w")) %>%
  group_by(method, group, criteria) %>%
  summarise(p = sum(value) / first(n)) %>%
  mutate(criteria = ifelse(is.na(criteria), "missing", criteria)) %>%
  filter(!is.na(group)) %>%
  spread(criteria, p) %>%
  arrange(group)

# summarise the criteria proportions overall for plants on the Red List
rl_criteria_count <-
  rl_criteria %>%
  select(`Species ID`, `Red List criteria`) %>%
  mutate(criteria_a = ifelse(is.na(`Red List criteria`), FALSE, 
                             str_detect(`Red List criteria`, "A")),
         criteria_b = ifelse(is.na(`Red List criteria`), FALSE, 
                             str_detect(`Red List criteria`, "B")),
         criteria_c = ifelse(is.na(`Red List criteria`), FALSE, 
                             str_detect(`Red List criteria`, "C")),
         criteria_d = ifelse(is.na(`Red List criteria`), FALSE, 
                             str_detect(`Red List criteria`, "D")),
         criteria_e = ifelse(is.na(`Red List criteria`), FALSE, 
                             str_detect(`Red List criteria`, "E")),
         missing = is.na(`Red List criteria`)) %>%
  select(-`Red List criteria`) %>%
  gather(criteria, value, -`Species ID`) %>%
  mutate(criteria = case_when(criteria == "missing" ~ "missing",
                              TRUE ~ str_extract(criteria, "(?<=\\_)\\w"))) %>%
  group_by(criteria) %>%
  summarise(n = sum(value)) %>%
  mutate(p = n / sum(n))

# analyse overall results -----------------------------------------------------

results_overall <- 
  results_data %>%
  group_by(method) %>%
  summarise(nobs = n(),
            n_correct = sum(obs == pred),
            accuracy = confusionMatrix(pred, obs)$overall["Accuracy"],
            default_accuracy = confusionMatrix(pred, obs)$overall["AccuracyNull"],
            accuracy_p_value = confusionMatrix(pred, obs)$overall["AccuracyPValue"],
            sensitivity = confusionMatrix(pred, obs)$byClass["Sensitivity"],
            specificity = confusionMatrix(pred, obs)$byClass["Specificity"])

# test for significance from the default accuracy

results_overall <-
  results_overall %>%
  group_by(method) %>%
  # update flat beta priors and draw from posterior
  mutate(samples = list(rbeta(10000, 1+n_correct, 1+nobs-n_correct))) %>%
  mutate(posterior_accuracy = map_dbl(samples, mean),
         ci_hi = map_dbl(samples, ~hdi(.x, 0.95)[2]),
         ci_lo = map_dbl(samples, ~hdi(.x, 0.95)[1]),
         significance = case_when(ci_lo > default_accuracy ~ "significant",
                                  TRUE ~ "not significant")) %>%
  select(-accuracy_p_value, -samples)

# analyse results by group ----------------------------------------------------

results_by_group <-
  results_data %>%
  group_by(method, group) %>%
  summarise(nobs = n(),
            n_correct = sum(obs == pred),
            accuracy = confusionMatrix(pred, obs)$overall["Accuracy"],
            default_accuracy = confusionMatrix(pred, obs)$overall["AccuracyNull"],
            accuracy_p_value = confusionMatrix(pred, obs)$overall["AccuracyPValue"],
            sensitivity = confusionMatrix(pred, obs)$byClass["Sensitivity"],
            specificity = confusionMatrix(pred, obs)$byClass["Specificity"]) %>%
  group_by(method, group) %>%
  mutate(samples = list(rbeta(10000, 1+n_correct, 1+nobs-n_correct))) %>%
  mutate(posterior_accuracy = map_dbl(samples, mean),
         ci_hi = map_dbl(samples, ~hdi(.x, 0.95)[2]),
         ci_lo = map_dbl(samples, ~hdi(.x, 0.95)[1]),
         significance = case_when(ci_lo > default_accuracy ~ "significant",
                                  TRUE ~ "not significant")) %>%
  select(-accuracy_p_value, -samples)

# pairwise differences between method results ---------------------------------

# tabulate true positives, etc.
confusion_table <-
  results_data %>%
  group_by(method) %>%
  summarise(tp = sum(obs == pred & pred == "not_threatened"),
            fp = sum(obs != pred & pred == "not_threatened"),
            tn = sum(obs == pred & pred == "threatened"),
            fn = sum(obs != pred & pred == "threatened"))

# model method performance metrics
confusion_samples <- 
  confusion_table %>%
  group_by(method) %>%
  # update flat dirichlet priors and draw samples form posterior
  mutate(samples = list(rdirichlet(10000, c(1+tp, 1+fp, 1+tn, 1+fn)) %>% 
                          as.tibble() %>% 
                          set_colnames(c("tp", "fp", "tn", "fn")))) %>%
  select(-tp, -fn, -tn, -fp) %>%
  unnest() %>%
  mutate(sensitivity = tp / (tp + fn), 
         specificity = tn / (tn + fp), 
         accuracy = (tp + tn) / (tp + fp + fn + tn)) %>%
  select(-tp, -fp, -tn, -fn)

# calculate pairwise differences
method_differences <-
  map_dfr(unique(confusion_samples$method), 
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
                         red_list_criteria = rl_criteria_count,
                         overall_results = results_overall,
                         results_by_group = results_by_group,
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
