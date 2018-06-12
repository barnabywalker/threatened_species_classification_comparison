###############################################################################
##### Script to plot the results of the method analysis and comparison.   #####
###############################################################################

library(here)
library(tidyverse)
library(readxl)
library(ggpubr)
library(jsonlite)

# load data ------------------------------------------------------------------
results_by_group <- 
  here("output", "method_comparison_and_analysis.xlsx") %>% 
  read_xlsx(sheet="results_by_group")

results_samples_summary <-
  here("output", "method_comparison_and_analysis.xlsx") %>% 
  read_xlsx(sheet="confusion_samples_summary")

overall_importance <-
  here("output", "method_comparison_and_analysis.xlsx") %>% 
  read_xlsx(sheet="predictor_importance")

importance_by_group <- 
  here("output", "method_comparison_and_analysis.xlsx") %>% 
  read_xlsx(sheet="predictor_importance_by_group")

rf_results <-
  here("output", "random_forests_results.csv") %>%
  read_csv()

us_method_steps <-
  here("output", "method_comparison_and_analysis.xlsx") %>% 
  read_xlsx(sheet="us_method_steps")

specimen_count_thresholds <-
  here("output", "specimen_count_threshold_accuracy.csv") %>% 
  read_csv()

criteria_accuracy <-
  here("output", "method_comparison_and_analysis.xlsx") %>% 
  read_xlsx(sheet="criteria_accuracy")

criteria_accuracy_by_group <-
  here("output", "method_comparison_and_analysis.xlsx") %>% 
  read_xlsx(sheet="criteria_accuracy_by_group")

# compare method performances ------------------------------------------------
bar_colours <- c('#bfd3e6', '#9ebcda', '#8c96c6', '#8856a7', '#810f7c')

# overall
method_comparison <-
  results_samples_summary %>%
  ungroup() %>%
  mutate(measure = factor(measure, levels=c("accuracy", "sensitivity", "specificity"), ordered=TRUE),
         method = fct_reorder(method, observed_value, desc=TRUE),
         significance = case_when(significance == "significant" & measure == "accuracy" ~ "*",
                                  TRUE ~ NA_character_),
         label = case_when(measure == "accuracy" ~ as.character(method),
                           TRUE ~ NA_character_)) %>%
  ggplot(mapping=aes(x=method, y=observed_value, fill=method)) +
  geom_bar(stat="identity", position="dodge") +
  geom_text(mapping=aes(label=significance, y=ci_hi+0.025, colour=method), size=4, vjust=0.75) +
  geom_errorbar(mapping=aes(x=method, ymin=ci_lo, ymax=ci_hi), width=0.5) +
  coord_flip() +
  facet_grid(measure~.) +
  theme_pubclean() +
  theme(panel.grid.major.y=element_blank(), 
        panel.grid.major.x=element_line(linetype=2, colour="grey"), 
        axis.text = element_text(size=8),
        axis.title = element_text(size=10),
        strip.text = element_text(size=8)) +
  scale_fill_manual(values=bar_colours, name="") +
  scale_colour_manual(values=bar_colours, name="") +
  scale_y_continuous(limits=c(0, 1.05), expand=c(0, 0)) +
  labs(x="", y="Value") +
  guides(fill=FALSE, colour=FALSE)

# by group
method_comparison_by_group <-
  results_by_group %>%
  ungroup() %>%
  mutate(significance = case_when(significance == "significant" ~ "*",
                                  TRUE ~ ""),
         group = paste("       ", group),
         group = factor(group, levels=rev(unique(group)), ordered=TRUE),
         method = fct_reorder(method, accuracy, desc=TRUE),
         thing = "a") %>%
  complete(method, group, fill=list(accuracy=0, significance="", ci_hi=NA, ci_lo=NA, thing="a")) %>%
  ggplot(mapping=aes(x=group, y=accuracy, fill=method)) +
  geom_bar(stat="identity", position="dodge") +
  geom_text(mapping=aes(label=significance, y=ci_hi+0.025, colour=method), size=4, vjust=0.75, position=position_dodge(width=0.9)) +
  geom_errorbar(mapping=aes(x=group, ymin=ci_lo, ymax=ci_hi), width=0.5, position=position_dodge(width=0.9)) +
  coord_flip() +
  theme_pubclean() +
  guides(fill=guide_legend(nrow=2), colour=FALSE) +
  theme(panel.grid.major.y=element_blank(), 
        panel.grid.major.x=element_line(linetype=2, colour="grey"),
        legend.position = "bottom",
        axis.text = element_text(size=8),
        axis.title = element_text(size=10),
        legend.text = element_text(size=7),
        legend.box.just = 1,
        strip.background = element_blank(),
        strip.text = element_text(colour="white"),
        legend.justification = c(0,0)) +
  scale_fill_manual(values=bar_colours, name="") +
  scale_colour_manual(values=bar_colours, name="") +
  scale_y_continuous(limits=c(0, 1.05), expand=c(0, 0)) +
  facet_grid(thing~.) +
  labs(x="", y="Accuracy")

combined_plot <- ggarrange(method_comparison, method_comparison_by_group, ncol=1, nrow=2, labels=c("A", "B"), font.label=list(size=10))

ggsave(here("figures", "method_comparison.png"), plot=combined_plot, dpi=600, width=3.5, height=6)

# random forests predictor importances --------------------------------------------------------------
colors <- rev(c('#e41a1c', '#377eb8', '#4daf4a', '#984ea3', '#ff7f00', '#999999', '#a65628', '#f781bf', "#bebada"))

predictor_list <- 
  importance_by_group %>%
  filter(rank <= 5) %>%
  mutate(predictor = factor(predictor, levels=rev(levels(reorder(overall_importance$predictor, overall_importance$rank))), ordered=TRUE)) %>%
  pull(predictor) %>%
  unique() %>%
  sort() %>%
  as.character()

overall_stick_plot <-
  overall_importance %>%
  mutate(colour = case_when(predictor %in% predictor_list ~ predictor,
                            TRUE ~ "black"),
         colour = factor(colour, levels=c("black", predictor_list), ordered=TRUE)) %>%
  ggplot(mapping=aes(x=reorder(predictor, mean_decrease_accuracy), y=mean_decrease_accuracy, colour=colour)) +
  geom_point() +
  geom_segment(mapping=aes(xend=reorder(predictor, mean_decrease_accuracy), yend=0)) +
  coord_flip() +
  scale_colour_manual(values=c("black", colors)) +
  guides(colour=FALSE) +
  labs(x="", y="Mean decrease in accuracy") +
  theme_pubclean() +
  theme(panel.grid.major.y=element_blank(), 
        panel.grid.major.x=element_line(linetype=2, colour="grey"),
        axis.text = element_text(size=8),
        axis.title = element_text(size=10))

top5_dot_plot <-
  importance_by_group %>%
  filter(rank <= 5) %>%
  ungroup() %>%
  mutate(group = factor(group, levels=rev(unique(group)), ordered=TRUE),
         predictor = factor(predictor, levels=predictor_list, ordered=TRUE)) %>%
  ggplot(mapping=aes(x=rank, y=group, label=predictor)) +
  geom_point(mapping=aes(size=mean_decrease_accuracy, colour=predictor)) +
  geom_text(mapping=aes(y=as.double(group) + mean_decrease_accuracy + 0.35), size=1.7) +
  guides(colour=FALSE,
         size=guide_legend(title.position="bottom", title.hjust=0.5, title.vjust=8)) +
  scale_colour_manual(values=colors) +
  scale_radius(range=c(1, 10), name="Mean decrease in accuracy") +
  scale_x_continuous(limits=c(0.75, 5.25)) +
  theme_pubclean() +
  theme(panel.grid.major.y=element_blank(),
        legend.position="bottom",
        axis.text = element_text(size=8),
        axis.title = element_text(size=10),
        legend.text = element_text(size=7),
        legend.title = element_text(size=8),
        legend.key = element_blank(),
        legend.margin=ggplot2::margin(0,0,0,0),
        legend.box.margin=ggplot2::margin(-10,-10,-10,-10)) +
  labs(y="", x="Predictor importance rank")

prob_boxplot <-
  rf_results %>%
  mutate(category = factor(category, levels=c("LC", "NT", "VU", "EN", "CR"), ordered=TRUE)) %>%
  ggplot(mapping=aes(x=category, y=prob, fill=category)) +
  stat_boxplot(geom = "errorbar", width=0.3) +
  geom_boxplot() +
  geom_hline(yintercept=0.5, linetype=2, colour="grey50") +
  scale_fill_brewer(palette="YlOrRd") +
  guides(fill=FALSE) + 
  labs(y="Predicted probability\nof threat", x="IUCN Red List category") +
  theme_pubclean() +
  theme(axis.text = element_text(size=8),
        axis.title = element_text(size=10))

rf_results_plot <- ggarrange(overall_stick_plot, top5_dot_plot, prob_boxplot, 
                             ncol=1, nrow=3, 
                             labels=c("A", "B", "C"), 
                             heights=c(1, 1.1, 0.8), 
                             font.label=list(size=10))


ggsave(here("figures", "random_forests_results.png"), plot=rf_results_plot, dpi=600, width=3.5, height=7.5)

# us method plot --------------------------------------------------------------

# This is much easier to make in D3 so just output the data in the right format

total <- sum(us_method_steps$n)
nodes = tibble(name=c("Temporal I", "Spatial", "Abundance", "Temporal II", "Not threatened", "Threatened"))

links = tibble(source=c(0, 1, 1, 2, 2, 3, 3),
               target=c(1, 2, 4, 3, 5, 4, 5),
               value=c(total - us_method_steps[1,]$n,
                       total - us_method_steps[1,]$n - us_method_steps[2,]$n,
                       us_method_steps[2,]$n,
                       total - us_method_steps[1,]$n - us_method_steps[2,]$n - us_method_steps[3,]$n,
                       us_method_steps[3,]$n,
                       us_method_steps[4,]$n,
                       us_method_steps[5,]$n))

list(nodes=nodes, links=links) %>% 
  toJSON() %>%
  write_lines(here("output", "us_method_flow_graph.json"))

# specimen count thresholds ---------------------------------------------------
best_threshold <-
  specimen_count_thresholds %>%
  arrange(desc(accuracy)) %>%
  pull(threshold) %>%
  head(1)

default_accuracy <-
  specimen_count_thresholds %>%
  pull(default_accuracy) %>%
  head(1)

specimen_count_plot <-
  specimen_count_thresholds %>%
  select(-precision, -recall) %>%
  gather(measure, value, -threshold, -default_accuracy, -accuracy_p_value) %>%
  ggplot(mapping=aes(x=threshold, y=value, colour=measure)) +
  geom_line() +
  geom_vline(xintercept=best_threshold, linetype=2, colour="grey50") +
  geom_hline(yintercept=default_accuracy, linetype=2, colour="grey50") +
  annotate("text", x=best_threshold+1, y=0, label="best threshold", hjust=0, colour="grey50") +
  annotate("text", x=100, y=default_accuracy+0.05, label="default accuracy", hjust=1, colour="grey50") +
  theme_pubclean() +
  scale_colour_brewer(palette="Set1", name="") +
  theme(legend.position = "bottom") +
  labs(y="Value", x="Specimen number threshold")

ggsave(here("figures", "specimen_count_threshold_accuracy.png"), plot=specimen_count_plot, dpi=600, width=6, height=3.5)

# criteria accuracies ---------------------------------------------------------
criteria_differences <-
  ggplot(data=criteria_accuracy, mapping=aes(x=method, y=mean_difference, colour=significance)) +
  geom_point() +
  geom_segment(mapping=aes(xend=method, y=difference_ci_hi, yend=difference_ci_lo)) +
  geom_hline(yintercept=0, linetype=2, colour="grey50") +
  coord_flip() +
  scale_colour_brewer(palette="Set1", name="") +
  labs(x="", y="Difference in accuracy") +
  theme_pubclean() +
  theme(panel.border = element_rect(fill=NA, colour="grey80")) +
  facet_grid(criteria~.)

ggsave(here("figures", "criteria_differences.png"), plot=criteria_differences, dpi=600, width=9, height=9)

criteria_differences_by_group <-
  ggplot(data=criteria_accuracy_by_group, mapping=aes(x=method, y=mean_difference, colour=significance)) +
  geom_point() +
  geom_segment(mapping=aes(xend=method, y=difference_ci_hi, yend=difference_ci_lo)) +
  geom_hline(yintercept=0, linetype=2, colour="grey50") +
  coord_flip() +
  scale_colour_brewer(palette="Set1", name="") +
  labs(x="", y="Difference in accuracy") +
  theme_pubclean() +
  theme(panel.border = element_rect(fill=NA, colour="grey80")) +
  facet_grid(group ~ criteria)

ggsave(here("figures", "criteria_differences_by_group.png"), plot=criteria_differences_by_group, dpi=600, width=6.5, height=9)
