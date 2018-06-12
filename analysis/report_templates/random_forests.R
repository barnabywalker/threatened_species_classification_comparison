#' ---
#' title: "`r report_title`"
#' author: "Barnaby Walker"
#' output: html_notebook
#' date: "`r report_date`"
#' ---
#' 
#' This is a notebook describing the results from a Random Forests model trained to predict whether a species is threatened or not,
#' using similar predictors to those described in [Bland et al](https://onlinelibrary.wiley.com/doi/abs/10.1111/cobi.12372).
#' 
#' [Back to overview](../results_overview_report.nb.html)
#' 
#+ include=FALSE
library(knitr)

info <- read_rds(here("output", "random_forests_info.rds"))
model <- read_rds(here("output", "random_forests_model.rds"))
train_set <- read_csv(here("output", "random_forests_train_set.csv"))
test_set <- read_csv(here("output", "random_forests_test_set.csv"))
results <- 
  here("output", "random_forests_results.csv") %>%
  read_csv() %>%
  mutate(obs = factor(obs),
         pred = factor(pred, levels=levels(obs)))

hi_correlations <- read_csv(here("output", "random_forests_correlated_predictors.csv"))
nzv_info <- read_csv(here("output", "random_forests_nzv_info.csv"))
scaler <- read_rds(here("output", "random_forests_scaler.rds"))

#' 
#' ## Model set up
#' 
#' The chosen split of train/test samples was **`r info$split_parameters$ratio`**.

#' These were the number of species in each group overall, and for the training and test sets.
#+ warning=FALSE, message=FALSE

train_set %>%
  mutate(set = "train") %>%
  rbind(test_set %>% mutate(set="test")) %>%
  group_by(group) %>%
  summary(n = n(),
          n_train = sum(set == "train"),
          n_test = sum(set == "test"),
          p_train = n_train / n,
          p_test = n_test / n) %>%
  kable()

#' The data was pre-processed by first centering and scaling all numerical predictors. Below are the predictors that were centered, scaled, or ignored by the scaler.
tibble(predictor=colnames(train_set)) %>%
  mutate(centered = predictor %in% scaler$method$scale,
         scaled = predictor %in% scaler$method$scale,
         ignored = predictor %in% scaler$method$ignore) %>%
  arrange(centered) %>%
  knitr::kable()

#' Any near-zero variance predictors were then removed from the data. Below are the removed predictors.
nzv_info %>%
  filter(nzv | zeroVar)

#' And finally any highly correlated predictors were removed. The chosen cutoff for high correlation was **`r info$preprocess_parameters$correlated_predictors_cutoff`**.

#' And these were the removed predictors.
hi_correlations

#' ## Tuning the model
#' 
#' The only hyperparameter tuned for the model was the `mtry` parameter, which is the number of randomly selected variables to chosen at each split in the tree. The values tested for this were:
info$method_parameters$hyperparameter_tuning_grid

#' This was tuned using **`r info$tuning_parameters$cv_method`** with **`r info$tuning_parameters$cv_folds` folds** and **`r info$tuning_parameters$cv_repeats` repeats**.
#' 
#' The number of trees in the random forest was kept constant at **`r info$method_parameters$n_trees` trees**.
#' 
#' The best model was chosen by **maximising the ROC**.
#' 
#' This is the tuning curve for the model.
model$results %>%
  gather(measure, value, -mtry, -ends_with("SD")) %>%
  gather(name, sd, -mtry, -measure, -value) %>%
  mutate(name = str_remove(name, "SD")) %>%
  filter(measure == name) %>%
  select(-name) %>%
  ggplot(mapping=aes(x=mtry, y=value, colour=measure, fill=measure)) +
  geom_line() +
  geom_point() +
  geom_ribbon(mapping=aes(ymin=value-sd, ymax=value+sd), alpha=0.3) +
  facet_wrap(~measure, nrow=3) +
  guides(colour=FALSE, fill=FALSE) +
  labs(title="Model hyperparameter tuning")

#' The **best value of mtry was `r model$bestTune[,"mtry"]`**.
#' 
#' ## Model accuracy
#' 
#' ### Training set
#' 
#' This is the distribution of probabilities predicted for the training set.
training_results <- 
  predict(model, model$trainingData, type="prob") %>%
  cbind(train_set %>% select(species, category, group)) %>%
  mutate(prob = threatened,
         obs = train_set$threatened,
         pred = predict(model, model$trainingData)) %>%
  select(-not_threatened)

ggplot(data=training_results, mapping=aes(x=prob, fill=obs, colour=obs)) +
  geom_density(alpha=0.5) +
  scale_fill_brewer(palette="Set1", name="") +
  scale_colour_brewer(palette="Set1", name="") +
  labs(x="p(threatened)", title="Training set probabilities") + 
  theme(legend.position="bottom")

#' The **OOB accuracy** of the model is **`r (model$finalModel$confusion["not_threatened", "not_threatened"] + model$finalModel$confusion["threatened", "threatened"]) / nrow(train_set)`**.
#' 
#' ### Test set
#' 
#' This is the distribution of probabilities for the test set.
ggplot(data=results, mapping=aes(x=prob, fill=obs, colour=obs)) +
  geom_density(alpha=0.5) +
  scale_fill_brewer(palette="Set1", name="") +
  scale_colour_brewer(palette="Set1", name="") +
  labs(x="p(threatened)", title="Test set probabilities") + 
  theme(legend.position="bottom")

#' The test set accuracy of the model is **`r sum(results$obs == results$pred) / nrow(results)`**.
#' 
#' The confusion matrix for the test set predictions gives a summary of where the missclassifications are happening.
test_confusion <- confusionMatrix(results$pred, results$obs)
test_confusion

#' This shows that the trained model **`r ifelse(test_confusion$overall["AccuracyPValue"] < 0.05, "is", "is not")` better than assigning the majority class**.
#' 
#' More visually, the confusion matrix is:
results %>%
  count(obs, pred) %>%
  ggplot(mapping=aes(x=obs,y=pred,fill=n)) +
  geom_tile() +
  geom_text(mapping=aes(label=n)) +
  scale_fill_distiller(palette="Blues", direction=1, name="") +
  labs(x="observed", y="predicted", title="Test set confusion matrix") +
  guides(fill=FALSE)

#' ### Predictor importances
#' 
#' The overall predictor importances are:
importances <- 
  importance(model$finalModel, type=1, scale=F) %>%
  as.data.frame() %>%
  mutate(var=rownames(.)) %>%
  arrange(desc(MeanDecreaseAccuracy))

ggplot(data=importances, mapping=aes(x=reorder(var, MeanDecreaseAccuracy), y=MeanDecreaseAccuracy)) +
  geom_segment(aes(xend=reorder(var, MeanDecreaseAccuracy), yend=0), color="grey50") +
  geom_point() +
  coord_flip() +
  labs(x="", y="Mean decrease in accuracy", title="Predictor importances")

#' ## Model performance on different groups
#' 
#' This is how the model performs on the test set when split into each of the separate groups.
summary_by_group <-
  results %>%
  group_by(group) %>%
  summarise(nobs = n(),
            accuracy = confusionMatrix(pred, obs)$overall["Accuracy"],
            default_accuracy = confusionMatrix(pred, obs)$overall["AccuracyNull"],
            accuracy_p_value = confusionMatrix(pred, obs)$overall["AccuracyPValue"],
            sensitivity = confusionMatrix(pred, obs)$byClass["Sensitivity"],
            specificity = confusionMatrix(pred, obs)$byClass["Specificity"])

summary_by_group %>% kable()

summary_by_group %>%
  gather(measure, value, -group, -nobs, -accuracy_p_value) %>%
  ggplot(mapping=aes(x=reorder(group, nobs), y=value, colour=group)) +
  geom_segment(aes(xend=reorder(group, nobs), yend=0), color="grey50") +
  geom_point() +
  coord_flip() +
  facet_grid(measure~.) +
  scale_colour_brewer(palette="Set1") +
  labs(x="", title="Model performance on test set by group") +
  guides(colour=FALSE)

#' The model did not perform significantly better on these groups:
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

#' And we can get the predictor importances for each data set separately.
#+ fig.width=9, fig.asp=2
model$finalModel$localImportance %>% 
  t() %>% 
  as.tibble() %>% 
  mutate(group = train_set$group) %>%
  gather(predictor, mda, -group) %>%
  group_by(group, predictor) %>%
  summarise(mda = mean(mda)) %>%
  ggplot(mapping=aes(x=reorder(predictor, mda), y=mda, colour=group)) +
  geom_point() +
  geom_segment(mapping=aes(xend=reorder(predictor, mda), yend=0), colour="grey50") +
  coord_flip() +
  facet_grid(group ~ .) +
  guides(colour=FALSE) +
  labs(x="", title="Predictor importances by group")

#' ## Model performance by Red List category
#' 
#' It might also be interesting to see how the model performed on species with different conservation assessments.
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

summary_by_category %>% kable()

summary_by_category %>%
  gather(measure, value, -category, -nobs, -accuracy_p_value) %>%
  ggplot(mapping=aes(x=category, y=value, colour=category)) +
  geom_segment(aes(xend=category, yend=0), color="grey50") +
  geom_point() +
  coord_flip() +
  facet_grid(measure~.) +
  scale_colour_brewer(palette="YlOrRd") +
  labs(x="", title="Model performance on test set by category") +
  guides(colour=FALSE)

#' And these are the predictor importances by category.
#+ fig.width=9, fig.asp=2
model$finalModel$localImportance %>% 
  t() %>% 
  as.tibble() %>% 
  mutate(category = factor(train_set$category, levels=categories, ordered=TRUE)) %>%
  gather(predictor, mda, -category) %>%
  group_by(category, predictor) %>%
  summarise(mda = mean(mda)) %>%
  ggplot(mapping=aes(x=reorder(predictor, mda), y=mda, colour=category)) +
  geom_point() +
  geom_segment(mapping=aes(xend=reorder(predictor, mda), yend=0), colour="grey50") +
  coord_flip() +
  facet_grid(category ~ .) +
  guides(colour=FALSE) +
  scale_colour_brewer(palette="YlOrRd") +
  labs(x="", title="Predictor importances by category")

#' And these are the predicted probabilites by category.

results %>%
  mutate(category = factor(category, levels=categories, ordered=TRUE)) %>%
  ggplot(mapping=aes(x=category, y=prob, fill=category)) +
  geom_boxplot() +
  scale_fill_brewer(palette="YlOrRd") +
  guides(fill=FALSE)
