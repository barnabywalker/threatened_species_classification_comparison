#############################################################################
##### Script to run analysis all the way through.                       #####
#############################################################################

library(here)

# run all the classification methods ----------------------------------------
source(here("analysis", "run_all_methods.R"))

# analyse and compare the methods -------------------------------------------
source(here("analysis", "analyse_results.R"))

# generate figures ----------------------------------------------------------
source(here("analysis", "plot_results.R"))
