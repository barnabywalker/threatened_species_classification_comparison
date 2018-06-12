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
library(knitr)

info <- read_rds(here("output", "specimen_count_info.rds"))

threshold_accuracies <- read_csv(here("output", "specimen_count_threshold_accuracy.csv"))

results <- 
  here("output", "specimen_count_results.csv") %>%
  read_csv() %>%
  mutate(obs = factor(obs),
         pred = factor(pred, levels=levels(obs)))

best_threshold <- results[1,]$threshold


#' 
#' ## Specimen count threshold
#' 
#' The best cutoff for the number of specimens was `r best_threshold`.
#' 
#' This is how the method metrics changed with the threshold.
default_accuracy  <- threshold_accuracies %>% head(1) %>% pull(default_accuracy) 

threshold_accuracies %>%
  select(-precision, -recall) %>%
  gather(measure, value, -threshold, -default_accuracy, -accuracy_p_value) %>%
  ggplot(mapping=aes(x=threshold, y=value, colour=measure)) +
  geom_line() +
  geom_vline(xintercept=best_threshold, linetype=2, colour="grey50") +
  geom_hline(yintercept=default_accuracy, linetype=2, colour="grey50") +
  annotate("text", x=best_threshold+1, y=0, label="best threshold", hjust=0, colour="grey50") +
  annotate("text", x=100, y=default_accuracy+0.03, label="baseline accuracy", hjust=1, colour="grey50") +
  theme(legend.position = "bottom") +
  labs(title="Change in method performance with specimen count threshold")

#' 
#' ## method accuracy
#' 
#' The method accuracy is given by the confusion matrix summary of the results.
confusion <- confusionMatrix(results$pred, results$obs)
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
  group_by(group) %>%
  summarise(nobs = n(),
            accuracy = confusionMatrix(pred, obs)$overall["Accuracy"],
            default_accuracy = confusionMatrix(pred, obs)$overall["AccuracyNull"],
            accuracy_p_value = confusionMatrix(pred, obs)$overall["AccuracyPValue"],
            sensitivity = confusionMatrix(pred, obs)$byClass["Sensitivity"],
            specificity = confusionMatrix(pred, obs)$byClass["Specificity"])

summary_by_group %>% knitr::kable()

summary_by_group %>%
  gather(measure, value, -group, -nobs, -accuracy_p_value) %>%
  ggplot(mapping=aes(x=reorder(group, nobs), y=value, colour=group)) +
  geom_segment(aes(xend=reorder(group, nobs), yend=0), color="grey50") +
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
  group_by(category) %>%
  summarise(nobs = n(),
            accuracy = confusionMatrix(pred, obs)$overall["Accuracy"],
            default_accuracy = confusionMatrix(pred, obs)$overall["AccuracyNull"],
            accuracy_p_value = confusionMatrix(pred, obs)$overall["AccuracyPValue"],
            sensitivity = confusionMatrix(pred, obs)$byClass["Sensitivity"],
            specificity = confusionMatrix(pred, obs)$byClass["Specificity"])

summary_by_category %>% knitr::kable()

summary_by_category %>%
  gather(measure, value, -category, -nobs, -accuracy_p_value) %>%
  ggplot(mapping=aes(x=category, y=value, colour=category)) +
  geom_segment(aes(xend=category, yend=0), color="grey50") +
  geom_point() +
  coord_flip() +
  facet_grid(measure~.) +
  scale_colour_brewer(palette="YlOrRd") +
  labs(x="", title="method performance on test set by category") +
  guides(colour=FALSE)

