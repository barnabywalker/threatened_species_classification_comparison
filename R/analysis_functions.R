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