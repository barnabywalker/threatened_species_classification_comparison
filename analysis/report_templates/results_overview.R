#' ---
#' title: "Results overview"
#' author: "Barnaby Walker"
#' output: html_notebook
#' date: "`r report_date`"
#' ---
#' 
#' This gives an overview of the results from running the different classification methods. More in depth analysis of the results for each approach can be found in:
#' 
#' #### Random forests trained on
#' * [All species together](individual_results/random_forests_all_species.nb.html)
#' * Each data set in turn: [legumes](individual_results/random_forests_legumes.nb.html), [Madagascar palms](individual_results/random_forests_madagascar_palms.nb.html), [Aulomyrcia](individual_results/random_forests_myrts.nb.html), [coffees](individual_results/random_forests_coffee.nb.html), and [New Guinea orchids](individual_results/random_forests_ng_orchids_p.nb.html).
#' * [Just the Red List species](individual_results/random_forests_redlist.nb.html).
#' 
#' #### [Krupnick's algorithm](individual_results/krupnick.nb.html)
#' 
#' #### [EOO assessments from rCAT](individual_results/rcat.nb.html)
#' 
#' #### [Criteria B assessments from ConR](individual_results/conr.nb.html)
#' 
#' #### [Putting a threshold on the number of specimens](individual_results/specimen_count.nb.html)
#' 
#+ include=FALSE
library(knitr)
library(gtools)
library(magrittr)
library(ggridges)
 
#' ## Results Summary
#' 
#' Below is a table summarising how each model performed.
#+ fig.width=8, warning=FALSE

results_table <- 
  all_results %>%
  group_by(method) %>%
  summarise(nobs = n(),
            accuracy = confusionMatrix(pred, obs)$overall["Accuracy"],
            default_accuracy = confusionMatrix(pred, obs)$overall["AccuracyNull"],
            accuracy_p_value = confusionMatrix(pred, obs)$overall["AccuracyPValue"],
            sensitivity = confusionMatrix(pred, obs)$byClass["Sensitivity"],
            specificity = confusionMatrix(pred, obs)$byClass["Specificity"])

results_table %>% kable()
results_table %>%
  mutate(label = case_when(accuracy_p_value <= 0.05 ~ "*",
                           TRUE ~ NA_character_)) %>%
  ggplot(mapping=aes(x=method, fill=method, y=accuracy)) + 
  geom_col(position="dodge") +
  geom_text(mapping=aes(label=label, colour=method, y=accuracy+0.02), position=position_dodge(width=0.9), size=6, vjust=0.65) +
  coord_flip() +
  theme(legend.position = "bottom") + 
  labs(x="", title="Comparison of model accuracies\n(significantly better than assigning the majority class indicated by '*')")

#' ## Results by group
#' 
#' The above compares the accuracy of each model overall, so below is a comparison of each model's performance on the different groups.

methods <- c("Random forests", "Krupnick", "ConR", "rCAT", "Specimen count")
colours <- c('#bfd3e6', '#9ebcda', '#8c96c6', '#8856a7', '#810f7c')
method_order <- results_table %>%
  arrange(accuracy) %>%
  pull(method)

per_group_results <-
  all_results %>%
  group_by(method, group) %>%
  summarise(nobs = n(),
            accuracy = confusionMatrix(pred, obs)$overall["Accuracy"],
            default_accuracy = confusionMatrix(pred, obs)$overall["AccuracyNull"],
            accuracy_p_value = confusionMatrix(pred, obs)$overall["AccuracyPValue"],
            sensitivity = confusionMatrix(pred, obs)$byClass["Sensitivity"],
            specificity = confusionMatrix(pred, obs)$byClass["Specificity"]) %>%
  filter(!is.na(group))

per_group_results %>% kable()

per_group_results %>%
  ungroup() %>%
  mutate(label = case_when(accuracy_p_value <= 0.05 ~ "*",
                           TRUE ~ NA_character_),
         method = factor(method, levels=method_order)) %>%
  complete(method, group, fill=list(accuracy=0, accuracy_p_value=NA, label=NA_character_)) %>%
  ggplot(mapping=aes(fill=method, x=group, y=accuracy)) + 
  geom_bar(stat="identity", position="dodge") +
  geom_text(mapping=aes(label=label, colour=method, y=accuracy+0.02), position=position_dodge(width=0.9), size=6, vjust=0.65) +
  coord_flip() +
  theme(legend.position = "bottom") + 
  labs(x="", title="Comparison of method accuracies on each group\n(significantly better than assigning the majority class indicated by '*')") +
  scale_colour_manual(values=colours, name="", drop=FALSE) +
  scale_fill_manual(values=colours, name="", drop=FALSE) +
  scale_x_discrete(drop=FALSE)

#' ## Are there significant differences in accuracy between methods?
#' 
#' We can model the proportion of false positives, true positives, etc. for each method as a multinomial distribution - each prediction is randomly assigned to one of the four categories.
#' From these modelled values, we can calculate modelled values for the accuracy, sensitivity, and specificity of each method.

measures_samples <-
  all_results %>%
  group_by(method) %>%
  summarise(tp = sum(obs == pred & pred == "not_threatened"),
            fp = sum(obs != pred & pred == "not_threatened"),
            tn = sum(obs == pred & pred == "threatened"),
            fn = sum(obs != pred & pred == "threatened")) %>%
  group_by(method) %>%
  mutate(samples = list(rdirichlet(10000, c(1 + tp, 1 + fp,1 + tn,1 + fn)) %>% as.tibble() %>% set_colnames(c("tp", "fp", "tn", "fn")))) %>%
  select(-tp, -fp, -tn, -fn) %>%
  unnest() %>%
  mutate(sensitivity = tp / (tp + fn), specificity = tn / (tn + fp), accuracy = (tp + tn) / (tp + fp + fn + tn)) %>%
  select(-tp, -fp, -tn, -fn) %>%
  ungroup() %>%
  mutate(method = factor(method, levels=method_order, ordered=TRUE))

measures_samples %>%
  gather(measure, value, -method) %>%
  ggplot(mapping=aes(x=value, y=method, fill=method)) +
  geom_density_ridges() +
  facet_grid(measure~.) +
  guides(fill=FALSE) +
  scale_fill_manual(values=colours) +
  labs(x="", y="", title="Posterior distributions of method metrics")

#' Above are the posterior distributions for the accuracy, sensitivity, and specificity. Below is a summary of the distributions showing the mean of each measure along with the 95 % CI.

measures_samples %>%
  gather(measure, value, -method) %>%
  group_by(method, measure) %>%
  summarise(mean = mean(value),
         ci_hi = quantile(value, 0.975),
         ci_lo = quantile(value, 0.025)) %>%
  ggplot(mapping=aes(x=method, y=mean, colour=method)) +
  geom_point() +
  geom_segment(mapping=aes(xend=method, y=ci_hi, yend=ci_lo)) +
  coord_flip() +
  facet_grid(measure~.) +
  scale_colour_manual(values=colours) +
  labs(x="", y="", title="Estimates of method metrics with 95% credible intervals") +
  guides(colour=FALSE)
  
#' ## How does criteria affect predictions?
#' 
#' All of the methods apart from the Krupnick method use the species range as a predictor, and is the majority of the ConR and rCAT results. 
#' Criteria B uses the range information, so are assessments made based on criteria B overrepresented in correct predictions?
#' In the data there are some assessments with more than one criteria, so all assessments with category B in them will be used here.
#' There are also some assessments with no criteria listed, which have been counted as not based on category B.
#' 
#' First let's look at how many B assessments there are in the group.
all_results %>%
  mutate(b_assessment = str_detect(criteria, "B")) %>%
  group_by(method, obs) %>%
  summarise(percent_b_assessments = sum(b_assessment, na.rm=TRUE) / n()) %>%
  spread(obs, percent_b_assessments) %>%
  ungroup() %>%
  knitr::kable()

#' The number of B assessments is much, much higher for threatened species. But this might be because of all the missing criteria.
all_results %>%
  group_by(method, obs) %>%
  summarise(percent_na_assessments = sum(is.na(criteria)) / n()) %>%
  spread(obs, percent_na_assessments) %>%
  ungroup() %>%
  knitr::kable()

#' Missing criteria are much more likely for not threatened species. So we will only look at threatened species, and the question becomes:
#'  given a species is threatened, is it more likely to be correctly classified if it has a criteria B assessment than if it doesn't?
#'  
#'  We'll answer this question by using a Bayesian binomial test of proportions - 
#'  use Bayesian parameter estimation to estimate the probability a threatened species is correctly identified. To do this, we'll model
#'  the process as binomial, and draw 10000 samples from the conjugate prior. We'll use a flat prior for this as well.

binomial_samples <-
  all_results %>%
  filter(obs == "threatened") %>%
  mutate(b_assessment = case_when(is.na(criteria) ~ FALSE,
                                   TRUE ~ str_detect(criteria, "B")),
         correct = obs == pred) %>%
  group_by(method) %>%
  summarise(n = n(),
            n_b = sum(b_assessment),
            n_notb = n - n_b,
            n_b_correct = sum(b_assessment & correct),
            n_notb_correct = sum(!b_assessment & correct)) %>%
  gather(criteria, n_correct, -method, -n, -n_b, -n_notb) %>%
  gather(criteria_2, n_total, -method, -n, -criteria, -n_correct) %>%
  mutate(criteria = str_extract(criteria, "(?<=_)[\\w]+(?=_)"),
         criteria_2 = str_extract(criteria_2, "(?<=_)[\\w]+$")) %>%
  filter(criteria == criteria_2) %>%
  select(-n, -criteria_2) %>%
  group_by(method, criteria) %>%
  mutate(samples = list(rbeta(10000, 1+n_correct, 1+(n_total - n_correct))))

binomial_samples %>%
  unnest() %>%
  ggplot(mapping=aes(x=samples, fill=criteria)) +
  geom_density(alpha=0.9) +
  facet_wrap(~method) +
  scale_fill_brewer(palette="YlGnBu") +
  labs(x="p(correct)", title="Posterior distribution of probabilities")

#' We can test for if the difference is significant by taking the difference between the posteriors and seeing if zero is within the 95 % CI.
binomial_samples %>%
  select(-n_correct, -n_total) %>%
  spread(criteria, samples) %>%
  unnest() %>%
  mutate(difference = b - notb,
         ci_hi = quantile(difference, 0.975),
         ci_lo = quantile(difference, 0.025)) %>%
  ggplot(mapping=aes(x=difference)) +
  geom_density(fill="steelblue", alpha=0.7) +
  geom_vline(xintercept = 0, linetype=2, colour="grey50") +
  geom_segment(mapping=aes(x=ci_lo, xend=ci_hi, y=0, yend=0), colour="red", size=2) +
  facet_wrap(~method) +
  labs(title="Posterior differences")

#' Which can be made clearer as a forest plot.

binomial_samples %>%
  select(-n_correct, -n_total) %>%
  spread(criteria, samples) %>%
  unnest() %>%
  mutate(difference = b - notb) %>%
  group_by(method) %>%
  summarise(mean_diff = mean(difference),
            ci_hi = quantile(difference, 0.975),
            ci_lo = quantile(difference, 0.025),
            significance = case_when(ci_lo < 0 & ci_hi > 0 ~ "not significant",
                                     TRUE ~ "significant")) %>%
  ggplot(mapping=aes(x=method, colour=significance)) +
  geom_segment(mapping=aes(xend=method, y=ci_lo, yend=ci_hi)) +
  geom_point(mapping=aes(y=mean_diff)) +
  geom_hline(yintercept=0, linetype=2, colour="grey50") +
  coord_flip() +
  scale_colour_brewer(palette = "Set1") +
  labs(y="difference", x="", title="Difference in posterior probability of correct classification")
