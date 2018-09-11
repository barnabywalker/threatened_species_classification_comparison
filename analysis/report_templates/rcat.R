#' ---
#' title: "`r report_title`"
#' author: "Barnaby Walker"
#' output: html_notebook
#' date: "`r report_date`"
#' ---
#' 
#' This is a notebook describing the results from classifying a species as threatened or not using the `r method_info$method` method.
#' 
#' [Back to overview](../results_overview_report.nb.html)
#' 
#+ include=FALSE
library(knitr)
source(here("R", "analysis_functions.R"))

info <- read_rds(here("output", "rcat_info.rds"))

results <- 
  here("output", "rcat_results.csv") %>%
  read_csv() %>%
  mutate(obs = factor(obs),
         pred = factor(pred, levels=levels(obs)))

#' ## method accuracy
#' 
#' The method accuracy is given by the confusion matrix summary of the results.
confusion <- confusionMatrix(results$pred, results$obs, positive="threatened")
confusion

#' This shows that the trained method **`r ifelse(confusion$overall["AccuracyPValue"] < 0.05, "is", "is not")` better than assigning the majority class**.
#' 
#' More visually, the confusion matrix is:
results %>%
  count(obs, pred) %>%
  ggplot(mapping=aes(x=obs,y=pred,fill=n)) +
  geom_tile() +
  geom_text(mapping=aes(label=n)) +
  scale_fill_distiller(palette="Blues", direction=1, name="") +
  labs(x="observed", y="predicted", title="Resylts confusion matrix") +
  guides(fill=FALSE)

#' ## method performance on different groups
#' 
#' This is how the method performs on the test set when split into each of the separate groups.
summary_by_group <-
  results %>%
  summarise_results_by(group, positive_case="threatened")

summary_by_group %>% knitr::kable()

summary_by_group %>%
  gather(measure, value, -group, -nobs, -accuracy_p_value, -lower_ci, -upper_ci, -default_accuracy) %>%
  mutate(lower_ci=ifelse(measure == "accuracy", lower_ci, NA),
         upper_ci=ifelse(measure == "accuracy", upper_ci, NA),
         default_accuracy=ifelse(measure == "accuracy", default_accuracy, NA),
         stick_start=ifelse(measure == "accuracy", NA, value)) %>%
  ggplot(mapping=aes(x=reorder(group, nobs), y=value, colour=group)) +
  geom_segment(mapping=aes(xend=reorder(group, nobs), yend=0, y=stick_start), colour="grey50") +
  geom_segment(mapping=aes(xend=reorder(group, nobs), y=lower_ci, yend=upper_ci), colour="black", size=1) +
  geom_point(mapping=aes(x=reorder(group, nobs), y=default_accuracy), pch=17) +
  geom_point() +
  coord_flip() +
  facet_grid(measure~.) +
  scale_colour_brewer(palette="Set1") +
  labs(x="", title="method performance on test set by group") +
  guides(colour=FALSE)

#' The method did not perform significantly better on these groups:
summary_by_group %>%
  filter(accuracy_p_value > 0.05)

#' We can see where each went wrong with the confusion matrix.
results %>%
  group_by(group, obs, pred) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  group_by(group) %>%
  mutate(p = n / n()) %>%
  complete(group, obs, pred, fill=list(n=0, p=0)) %>%
  ggplot(mapping=aes(x=obs, y=pred, fill=p)) +
  geom_tile() +
  geom_text(mapping=aes(label=n)) +
  scale_fill_distiller(palette="Blues", direction=1, name="") +
  facet_wrap(~group)+
  labs(x="observed", y="predicted", title="Test set confusion matrix by group") +
  guides(fill=FALSE)

#' ## method performance by Red List category
#' 
#' It might also be interesting to see how the method performed on species with different conservation assessments.
categories <- c("LC", "NT", "VU", "EN", "CR")
summary_by_category <-
  results %>%
  mutate(category = factor(category, levels=categories, ordered=TRUE)) %>%
  summarise_results_by(category, positive_case="threatened")

summary_by_category %>% knitr::kable()

summary_by_category %>%
  gather(measure, value, -category, -nobs, -accuracy_p_value, -lower_ci, -upper_ci, -default_accuracy) %>%
  mutate(lower_ci=ifelse(measure == "accuracy", lower_ci, NA),
         upper_ci=ifelse(measure == "accuracy", upper_ci, NA),
         default_accuracy=ifelse(measure == "accuracy", default_accuracy, NA),
         stick_start=ifelse(measure == "accuracy", NA, value)) %>%
  ggplot(mapping=aes(x=category, y=value, colour=category)) +
  geom_segment(mapping=aes(xend=category, yend=0, y=stick_start), colour="grey50") +
  geom_segment(mapping=aes(xend=category, y=lower_ci, yend=upper_ci), colour="black", size=1) +
  geom_point() +
  coord_flip() +
  facet_grid(measure~.) +
  scale_colour_brewer(palette="YlOrRd") +
  labs(x="", title="method performance on test set by category") +
  guides(colour=FALSE)

