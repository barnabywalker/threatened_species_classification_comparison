#' ---
#' title: "`r report_title`"
#' author: "Barnaby Walker"
#' output: html_notebook
#' date: "`r report_date`"
#' ---
#' 
#' This is a notebook describing the results from classifying a species as threatened or not using the `r method_info$type` method.
#' 
#' [Back to overview](../results_overview_report.nb.html)
#' 
#+
library(knitr)
source(here("R", "analysis_functions.R"))

info <- 
  here("output", "us_method_info.rds") %>%
  read_rds()

results <- 
  here("output", paste(us_method_info$save_file, "_results.csv", sep="")) %>%
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
  labs(x="observed", y="predicted", title="Results confusion matrix") +
  guides(fill=FALSE)

#' ## method performance on different groups
#' 
#' This is how the method performs on the test set when split into each of the separate groups.
#+ warning=FALSE, message=FALSE
summary_by_group <-
  results %>%
  summarise_results_by(group, positive_case="threatened")

summary_by_group %>% kable()

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

#' We can also look at where each species was classified in the method flow.
steps <- c("temporal1", "spatial", "abundance", "temporal2", "none")

results %>%
  group_by(group) %>%
  summarise(a = sum(temporal1) / n(),
            b = sum(!temporal1 & spatial) / n(),
            c = sum(!temporal1 & !spatial & abundance) / n(),
            d = sum(!temporal1 & !spatial & !abundance & temporal2) / n(),
            none = sum(!temporal1 & !spatial & !abundance & !temporal2) / n()) %>%
  gather(step, n, -group) %>%
  mutate(step = recode(step,
                       a = "temporal1",
                       b = "spatial",
                       c = "abundance",
                       d = "temporal2"),
         step = factor(step, levels=rev(steps), ordered=TRUE)) %>%
  ggplot(mapping=aes(x=step, y=n, colour=step)) +
  geom_point(position=position_dodge(width=1)) +
  geom_segment(mapping=aes(xend=step, yend=0), colour="grey50") +
  coord_flip() +
  facet_grid(group ~ .) +
  guides(colour=FALSE)

#' And what the accuracy of each step was.
results %>%
  mutate(step = case_when(temporal1 ~ "temporal1",
                          spatial ~ "spatial",
                          abundance ~ "abundance",
                          temporal2 ~ "temporal2",
                          TRUE ~ "none")) %>%
  summarise_results_by(group, step, positive_case="threatened") %>%
  mutate(step = factor(step, levels=rev(steps), ordered=TRUE)) %>%
  complete(group, step, fill=list(accuracy=NA)) %>%
  ggplot(mapping=aes(x=step, y=accuracy, colour=step)) +
  geom_segment(mapping=aes(xend=step, y=lower_ci, yend=upper_ci), colour="black", size=1) +
  geom_point(cex=2) +
  coord_flip() +
  facet_grid(group ~ .) +
  guides(colour=FALSE) +
  labs(x="")

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

#' And where each category was classified in the method flow.
results %>%
  mutate(category = factor(category, levels=categories, ordered=TRUE)) %>%
  group_by(category) %>%
  summarise(a = sum(temporal1) / n(),
            b = sum(!temporal1 & spatial) / n(),
            c = sum(!temporal1 & !spatial & abundance) / n(),
            d = sum(!temporal1 & !spatial & !abundance & temporal2) / n(),
            none = sum(!temporal1 & !spatial & !abundance & !temporal2) / n()) %>%
  gather(step, n, -category) %>%
  mutate(step = recode(step,
                       a = "temporal1",
                       b = "spatial",
                       c = "abundance",
                       d = "temporal2"),
         step = factor(step, levels=rev(steps), ordered=TRUE)) %>%
  ggplot(mapping=aes(x=step, y=n, colour=step)) +
  geom_point(position=position_dodge(width=1)) +
  geom_segment(mapping=aes(xend=step, yend=0), colour="grey50") +
  coord_flip() +
  facet_grid(category ~ .) +
  guides(colour=FALSE)

#' And finally, where each category gets classified for each group.
d <- "MadPalms"
results %>%
  mutate(category = factor(category, levels=categories, ordered=TRUE)) %>%
  filter(group == d) %>%
  group_by(category) %>%
  summarise(a = sum(temporal1) / n(),
            b = sum(!temporal1 & spatial) / n(),
            c = sum(!temporal1 & !spatial & abundance) / n(),
            d = sum(!temporal1 & !spatial & !abundance & temporal2) / n(),
            none = sum(!temporal1 & !spatial & !abundance & !temporal2) / n()) %>%
  gather(step, n, -category) %>%
  mutate(step = recode(step,
                       a = "temporal1",
                       b = "spatial",
                       c = "abundance",
                       d = "temporal2"),
         step = factor(step, levels=rev(steps), ordered=TRUE)) %>%
  ggplot(mapping=aes(x=step, y=n, colour=step)) +
  geom_point(position=position_dodge(width=1)) +
  geom_segment(mapping=aes(xend=step, yend=0), colour="grey50") +
  coord_flip() +
  facet_grid(category ~ .) +
  guides(colour=FALSE) +
  labs(title=d)


d <- "Myrcia"
results %>%
  mutate(category = factor(category, levels=categories, ordered=TRUE)) %>%
  filter(group == d) %>%
  group_by(category) %>%
  summarise(a = sum(temporal1) / n(),
            b = sum(!temporal1 & spatial) / n(),
            c = sum(!temporal1 & !spatial & abundance) / n(),
            d = sum(!temporal1 & !spatial & !abundance & temporal2) / n(),
            none = sum(!temporal1 & !spatial & !abundance & !temporal2) / n()) %>%
  gather(step, n, -category) %>%
  mutate(step = recode(step,
                       a = "temporal1",
                       b = "spatial",
                       c = "abundance",
                       d = "temporal2"),
         step = factor(step, levels=rev(steps), ordered=TRUE)) %>%
  ggplot(mapping=aes(x=step, y=n, colour=step)) +
  geom_point(position=position_dodge(width=1)) +
  geom_segment(mapping=aes(xend=step, yend=0), colour="grey50") +
  coord_flip() +
  facet_grid(category ~ .) +
  guides(colour=FALSE) +
  labs(title=d)


d <- "OrchidsNG"
results %>%
  mutate(category = factor(category, levels=categories, ordered=TRUE)) %>%
  filter(group == d) %>%
  group_by(category) %>%
  summarise(a = sum(temporal1) / n(),
            b = sum(!temporal1 & spatial) / n(),
            c = sum(!temporal1 & !spatial & abundance) / n(),
            d = sum(!temporal1 & !spatial & !abundance & temporal2) / n(),
            none = sum(!temporal1 & !spatial & !abundance & !temporal2) / n()) %>%
  gather(step, n, -category) %>%
  mutate(step = recode(step,
                       a = "temporal1",
                       b = "spatial",
                       c = "abundance",
                       d = "temporal2"),
         step = factor(step, levels=rev(steps), ordered=TRUE)) %>%
  ggplot(mapping=aes(x=step, y=n, colour=step)) +
  geom_point(position=position_dodge(width=1)) +
  geom_segment(mapping=aes(xend=step, yend=0), colour="grey50") +
  coord_flip() +
  facet_grid(category ~ .) +
  guides(colour=FALSE) +
  labs(title=d)
