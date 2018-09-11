#############################################################################
##### Script to run all methods on the available data. This includes    #####
##### any necessary pre-processing, as well as generating summary       #####
##### report notebooks.                                                 #####
#############################################################################

library(here)
source(here("R", "classification_functions.R"))

library(tidyverse)

RANDOM_SEED <- 42

# Run random forests ---------------------------------------------------
print("Running random forests classification...")

rf_method_info <- list(
  method = "Random forests",
  data_set = "all_species",
  data_path = here("output", "rf_predictors_20180903.csv"),
  save_file = "random_forests",
  report_file = "random_forests",
  tune_method = TRUE,
  split_parameters = list(
    ratio = 0.75
  ),
  tuning_parameters = list(
    cv_method = "repeatedcv",
    cv_folds = 10,
    cv_repeats = 5
  ),
  preprocess_parameters = list(
    remove_correlated = TRUE,
    remove_nzv = TRUE,
    standardise = TRUE,
    correlated_predictors_cutoff = 0.75,
    clean_locations = FALSE,
    clean_coordinates = FALSE
  ),
  method_parameters = list(
    n_trees = 500,
    hyperparameter_tuning_grid = expand.grid(mtry = seq(1:12)) 
  )
)

random_forests_results <- run_classification(rf_method_info, 
                                             report_file=rf_method_info$report_file, 
                                             save_file=rf_method_info$save_file, 
                                             seed=RANDOM_SEED)

# Run US method ---------------------------------------------------------------
print("Running US method classification...")

us_method_info <- list(
  method = "US method",
  data_set = "all_species",
  data_path = here("output", "specimen_predictors_20180831.csv"),
  save_file = "us_method",
  report_file = "us_method",
  preprocess_parameters = list(
    clean_locations = TRUE,
    locality_level = 1,
    standardise = FALSE,
    remove_nzv = FALSE,
    remove_correlated = FALSE,
    clean_coordinates=FALSE
  )
)

us_method_results <- run_classification(us_method_info, 
                                        report_file=us_method_info$report_file, 
                                        save_file=us_method_info$save_file, 
                                        seed=RANDOM_SEED)

# Run rCAT method -------------------------------------------------------------
print("Running rCAT classification...")

rcat_method_info <- list(
  method = "rCAT",
  data_set = "all_species",
  data_path = here("output", "specimen_predictors_20180831.csv"),
  save_file = "rcat",
  report_file = "rcat",
  preprocess_parameters = list(
    clean_locations = FALSE,
    standardise = FALSE,
    remove_nzv = FALSE,
    remove_correlated = FALSE,
    clean_coordinates=TRUE
  )
)

rcat_results <- run_classification(rcat_method_info, 
                                   report_file=rcat_method_info$report_file, 
                                   save_file=rcat_method_info$save_file,  
                                   seed=RANDOM_SEED)

# Run ConR method -------------------------------------------------------------
print("Running ConR classification...")

conr_method_info <- list(
  method = "ConR",
  data_path = here("output", "specimen_predictors_20180831.csv"),
  save_file = "conr",
  report_file = "conr",
  preprocess_parameters = list(
    clean_locations = FALSE,
    standardise = FALSE,
    remove_nzv = FALSE,
    remove_correlated = FALSE,
    clean_coordinates=TRUE
  )
)

conr_results <- run_classification(conr_method_info, 
                                   report_file=conr_method_info$report_file, 
                                   save_file=conr_method_info$save_file,  
                                   seed=RANDOM_SEED)

# Run specimen count method ---------------------------------------------------
print("Running Specimen count classification...")

specimen_count_method_info <- list(
  method = "Specimen count",
  data_path = here("output", "specimen_predictors_20180831.csv"),
  save_file = "specimen_count",
  report_file = "specimen_count",
  preprocess_parameters = list(
    clean_locations = FALSE,
    standardise = FALSE,
    remove_nzv = FALSE,
    remove_correlated = FALSE,
    clean_coordinates=TRUE
  ),
  threshold_measure="accuracy"
)

specimen_count_results <- run_classification(specimen_count_method_info, 
                                             report_file=specimen_count_method_info$report_file, 
                                             save_file=specimen_count_method_info$save_file, 
                                             seed=RANDOM_SEED)


# Save final results of everything --------------------------------------------
all_results <-
  random_forests_results %>%
  rbind(us_method_results) %>%
  rbind(rcat_results) %>%
  rbind(conr_results) %>%
  rbind(specimen_count_results)

write_csv(all_results, here("output", "all_results_summary.csv"))

# Generate final report -------------------------------------------------------
report_date <- Sys.Date()
rmarkdown::render(here("analysis", "report_templates", "results_overview.R"), 
                  output_file=here("notebooks", "results_notebooks", "results_overview_report.nb.html"))
