#' ---
#' title: "US method with different localities"
#' author: "Barnaby Walker"
#' output: html_notebook
#' date: "`r report_date`"
#' ---
#' 
#' This gives an overview of the results from running the US method with different GADM locality levels. 
#' 
#' Administrative levels differ widely between countries, so the GADM dataset is an attempt to provide
#' a standardised set. Level 0 is countries, but the type of administrative zone is different for each country at the
#' higher levels. For Brazil, level 1 is states, level 2 is município, level 3 is distrito. 
#' For Madagascar, level 1 is provinces (Faritany mizakatena), level 2 is regions (Faritra), and level 3 is districts (Fivondronana).
#' For New Guinea, level 1 is regions and level 2 is districts. 
#' 
#' Not all countries have all administrative levels. Where a country was missing the desired administrative level, the next highest
#' level that it did have was used.
#' 
#+ include=FALSE
library(knitr)
library(gtools)
library(magrittr)
library(ggridges)
source(here("R", "analysis_functions.R"))

#' ## Results Summary
#' 
#' Below is a table summarising how each model performed.
#+ fig.width=8, warning=FALSE

colours <- c('#bfd3e6', '#9ebcda', '#8c96c6', '#8856a7', '#810f7c')

results_table <- 
  all_results %>%
  summarise_results_by(locality_level, positive_case="threatened")

results_table %>% 
  select(-upper_ci, -lower_ci) %>%
  kable()

results_table %>%
  mutate(label = case_when(accuracy_p_value <= 0.05 ~ "*",
                           TRUE ~ NA_character_),
         locality_level = factor(locality_level, ordered=TRUE)) %>%
  ggplot(mapping=aes(x=locality_level, fill=locality_level, y=accuracy)) + 
  geom_col(position="dodge") +
  geom_errorbar(mapping=aes(ymin=lower_ci, ymax=upper_ci), width=0.25, size=1) +
  geom_hline(mapping=aes(yintercept=default_accuracy), linetype=2, colour="red", size=1) +
  geom_text(mapping=aes(label=label, y=upper_ci+0.02), size=6, vjust=0.7, show.legend=FALSE) +
  coord_flip() +
  theme(legend.position = "bottom") + 
  scale_fill_manual(values=colours, name="") +
  guides(fill=FALSE) +
  labs(x="Locality level",
       y="Accuracy",
       title="Comparison of model accuracy at different locality levels
(significantly better than assigning the default accuracy indicated by '*')")

#' ## Results by group
#' 
#' Below is a comparison of how the US method fares on each group.

per_group_results <-
  all_results %>%
  summarise_results_by(locality_level, group, positive_case="threatened") %>%
  filter(!is.na(group))

per_group_results %>% 
  select(-ends_with("ci")) %>%
  kable()

per_group_results %>%
  mutate(label = case_when(accuracy_p_value <= 0.05 ~ "*",
                           TRUE ~ NA_character_),
         locality_level = factor(locality_level, ordered=TRUE),
         group=factor(group, ordered=TRUE)) %>%
  ggplot(mapping=aes(fill=locality_level, x=group, y=accuracy)) + 
  geom_bar(stat="identity", position="dodge") +
  geom_errorbar(mapping=aes(ymin=lower_ci, ymax=upper_ci), width=0.5, size=1, position=position_dodge(width=0.9)) +
  geom_segment(mapping=aes(y=default_accuracy, yend=default_accuracy, xend=as.integer(group) + 0.45, x=as.integer(group) - 0.45), 
               linetype=2, colour="red", size=1) +
  geom_text(mapping=aes(label=label, y=upper_ci+0.02), position=position_dodge(width=0.9), size=6, vjust=0.7) +
  coord_flip() +
  theme(legend.position = "bottom") + 
  labs(x="", 
       title="Comparison of method accuracies on each group
(significantly better than assigning default accuracy indicated by '*')") +
  scale_fill_manual(values=colours, name="Locality level")

#' ## Are there significant differences in accuracy between methods?
#' 
#' We can model the proportion of false positives, true positives, etc. for each method as a multinomial distribution - each prediction is randomly assigned to one of the four categories.
#' From these modelled values, we can calculate modelled values for the accuracy, sensitivity, and specificity of each method.

measures_samples <-
  all_results %>%
  model_metrics_by(locality_level) %>%
  mutate(locality_level = factor(locality_level, ordered=TRUE))
  

measures_samples %>%
  gather(measure, value, -locality_level) %>%
  ggplot(mapping=aes(x=value, y=locality_level, fill=locality_level)) +
  geom_density_ridges() +
  facet_grid(measure~.) +
  guides(fill=FALSE) +
  scale_fill_manual(values=colours) +
  scale_x_continuous(limits=c(0, 1)) +
  labs(x="", y="", title="Posterior distributions of method metrics")

#' Above are the posterior distributions for the accuracy, sensitivity, and specificity. Below is a summary of the distributions showing the mean of each measure along with the 95 % CI.

measures_samples %>%
  gather(measure, value, -locality_level) %>%
  group_by(locality_level, measure) %>%
  summarise(mean = mean(value),
            ci_hi = quantile(value, 0.975),
            ci_lo = quantile(value, 0.025)) %>%
  ggplot(mapping=aes(x=locality_level, y=mean, colour=locality_level)) +
  geom_segment(mapping=aes(xend=locality_level, y=ci_hi, yend=ci_lo), colour="black", size=1) +
  geom_point(cex=3) +
  coord_flip() +
  facet_grid(measure~.) +
  scale_colour_manual(values=colours, name="Locality level") +
  labs(x="", y="", title="Estimates of method metrics with 95% credible intervals") +
  theme(legend.position="bottom")

