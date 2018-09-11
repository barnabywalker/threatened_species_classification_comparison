#############################################################################
##### Script to run US method for all standardised locality levels.     #####
#############################################################################

library(here)
source(here("R", "classification_functions.R"))

library(tidyverse)

RANDOM_SEED <- 123

# Set up overall run ----------------------------------------------------------
print("Running US method classification...")

us_method_info <- list(
  method = "US method",
  data_set = "all_species",
  data_path = here("output", "specimen_predictors_20180831_1810.csv"),
  preprocess_parameters = list(
    clean_locations = TRUE,
    standardise = FALSE,
    remove_nzv = FALSE,
    remove_correlated = FALSE,
    clean_coordinates=FALSE
  )
)

locality_level <- 3

# Loop through locality levels ------------------------------------------------

results_list <- list()

while (locality_level >= 0) {
  
  us_method_info$preprocess_parameters$locality_level <- locality_level
  us_method_info$save_file <- sprintf("us_method_locality%d", locality_level)
  us_method_info$report_file <- sprintf("us_method_locality%d", locality_level)
  
  us_method_results <- run_classification(us_method_info, 
                                          report_file=us_method_info$save_file,
                                          save_file=us_method_info$report_file,
                                          seed=RANDOM_SEED)
  
  results_list[[locality_level+1]] <- mutate(us_method_results, locality_level=locality_level)
  
  locality_level <- locality_level - 1
}

# Save final results of everything --------------------------------------------
all_results <-
  bind_rows(results_list)

write_csv(all_results, here("output", "us_method_locality_results_summary.csv"))

# Generate final report -------------------------------------------------------
report_date <- Sys.Date()
rmarkdown::render(here("analysis", "report_templates", "us_localities_overview.R"), 
                  output_file=here("notebooks", "results_notebooks", "us_localities_with_missing_coords_report.nb.html"))
