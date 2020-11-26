### ------------------------------------------------------
### analysis of incentivization experiment - figures
### ------------------------------------------------------


# define dv labels
outcome_labels_df <- data.frame(
  outcome = c("app_track_installed_at_w3_incent_std", "app_hyb_installed_at_w3_incent_std", "app_rep_installed_at_w3_incent_std", "app_know_index_std.3", "att_scale_score_incent_std.3", "app_sharing_clicks_std.2"),
  outcome_label = c("Uptake (tracked)", "Uptake (hybrid)", "Uptake (reported)",  "Knowledge", "Positive attitudes", "Message sharing (tracked)"))


### Coefplot for main results of incentivization experiment -------------------

# tidy DIM results
dim_incent_tc_pooled <- list(h_incent_2a_pooled$dim, h_incent_2ah_pooled$dim, h_incent_3a_pooled$dim, h_incent_4_pooled$dim, h_incent_5_pooled$dim, h_incent_6_pooled$dim) %>% map(tidy) %>% bind_rows() %>% filter(term == "treat_incent_any.2") %>% mutate(trt = "treat_incent_any.2", Treatment = "Pooled") %>% merge(outcome_labels_df, by = "outcome")
dim_incent_tc_eur1 <- list(h_incent_2b_eur1$dim, h_incent_2bh_eur1$dim, h_incent_3b_eur1$dim, h_incent_4_eur1$dim, h_incent_5_eur1$dim, h_incent_6_eur1$dim) %>% map(tidy) %>% bind_rows() %>% filter(term == "treat_incent_any.2") %>% mutate(trt = "treat_incent_any.2", Treatment = "EUR 1") %>% merge(outcome_labels_df, by = "outcome")
dim_incent_tc_eur2 <- list(h_incent_2b_eur2$dim, h_incent_2bh_eur2$dim, h_incent_3b_eur2$dim, h_incent_4_eur2$dim, h_incent_5_eur2$dim) %>% map(tidy) %>% bind_rows() %>% filter(term == "treat_incent_any.2") %>% mutate(trt = "treat_incent_any.2", Treatment = "EUR 2") %>% merge(outcome_labels_df, by = "outcome") # h_incent_6_eur2 could not be estimated
dim_incent_tc_eur5 <- list(h_incent_2b_eur5$dim, h_incent_2bh_eur5$dim, h_incent_3b_eur5$dim, h_incent_4_eur5$dim, h_incent_5_eur5$dim, h_incent_6_eur5$dim) %>% map(tidy) %>% bind_rows() %>% filter(term == "treat_incent_any.2") %>% mutate(trt = "treat_incent_any.2", Treatment = "EUR 5") %>% merge(outcome_labels_df, by = "outcome")


# tidy ITT results
itt_incent_tc_pooled <- list(h_incent_2a_pooled$itt[[1]], h_incent_2ah_pooled$itt[[1]], h_incent_3a_pooled$itt[[1]], h_incent_4_pooled$itt[[1]], h_incent_5_pooled$itt[[1]], h_incent_6_pooled$itt[[1]]) %>% map(tidy) %>% bind_rows() %>% filter(term == "treat_incent_any.2TRUE") %>% mutate(trt = "treat_incent_any.2", Treatment = "Pooled") %>% merge(outcome_labels_df, by = "outcome")
itt_incent_tc_eur1 <- list(h_incent_2b_eur1$itt[[1]], h_incent_2bh_eur1$itt[[1]], h_incent_3b_eur1$itt[[1]], h_incent_4_eur1$itt[[1]], h_incent_5_eur1$itt[[1]], h_incent_6_eur1$itt[[1]]) %>% map(tidy) %>% bind_rows() %>% filter(term == "treat_incent_any.2TRUE") %>% mutate(trt = "treat_incent_any.2", Treatment = "EUR 1") %>% merge(outcome_labels_df, by = "outcome")
itt_incent_tc_eur2 <- list(h_incent_2b_eur2$itt[[1]], h_incent_2bh_eur2$itt[[1]], h_incent_3b_eur2$itt[[1]], h_incent_4_eur2$itt[[1]], h_incent_5_eur2$itt[[1]]) %>% map(tidy) %>% bind_rows() %>% filter(term == "treat_incent_any.2TRUE") %>% mutate(trt = "treat_incent_any.2", Treatment = "EUR 2") %>% merge(outcome_labels_df, by = "outcome") # h_incent_6_eur2 could not be estimated
itt_incent_tc_eur5 <- list(h_incent_2b_eur5$itt[[1]], h_incent_2bh_eur5$itt[[1]], h_incent_3b_eur5$itt[[1]], h_incent_4_eur5$itt[[1]], h_incent_5_eur5$itt[[1]], h_incent_6_eur5$itt[[1]]) %>% map(tidy) %>% bind_rows() %>% filter(term == "treat_incent_any.2TRUE") %>% mutate(trt = "treat_incent_any.2", Treatment = "EUR 5") %>% merge(outcome_labels_df, by = "outcome")

itt_tc_all <- bind_rows(itt_incent_tc_pooled, itt_incent_tc_eur1, itt_incent_tc_eur2, itt_incent_tc_eur5)
itt_tc_all$Treatment <- factor(itt_tc_all$Treatment, levels = c("Pooled", "EUR 1", "EUR 2", "EUR 5"))


# tidy CACE results
cace_incent_tc_pooled <- list(h_incent_4_pooled$cace, h_incent_5_pooled$cace, h_incent_6_pooled$cace) %>% map(tidy) %>% bind_rows() %>% filter(term == "app_hyb_installed_at_w3TRUE") %>% mutate(trt = "treat_incent_any.2", Treatment = "Pooled") %>% merge(outcome_labels_df, by = "outcome")
cace_incent_tc_eur1 <- list(h_incent_2b_eur1$cace, h_incent_2bh_eur1$cace, h_incent_3b_eur1$cace, h_incent_4_eur1$cace, h_incent_5_eur1$cace, h_incent_6_eur1$cace) %>% map(tidy) %>% bind_rows() %>% filter(term == "app_hyb_installed_at_w3TRUE") %>% mutate(trt = "treat_incent_any.2", Treatment = "EUR 1") %>% merge(outcome_labels_df, by = "outcome")
cace_incent_tc_eur2 <- list(h_incent_2b_eur2$cace, h_incent_2bh_eur2$cace, h_incent_3b_eur2$cace, h_incent_4_eur2$cace, h_incent_5_eur2$cace) %>% map(tidy) %>% bind_rows() %>% filter(term == "app_hyb_installed_at_w3TRUE") %>% mutate(trt = "treat_incent_any.2", Treatment = "EUR 2") %>% merge(outcome_labels_df, by = "outcome") # h_incent_6_eur2 could not be estimated
cace_incent_tc_eur5 <- list(h_incent_2b_eur5$cace, h_incent_2bh_eur5$cace, h_incent_3b_eur5$cace, h_incent_4_eur5$cace, h_incent_5_eur5$cace, h_incent_6_eur5$cace) %>% map(tidy) %>% bind_rows() %>% filter(term == "app_hyb_installed_at_w3TRUE") %>% mutate(trt = "treat_incent_any.2", Treatment = "EUR 5") %>% merge(outcome_labels_df, by = "outcome")

cace_tc_all <- bind_rows(cace_incent_tc_pooled, cace_incent_tc_eur1, cace_incent_tc_eur2, cace_incent_tc_eur5)
cace_tc_all$Treatment <- factor(cace_tc_all$Treatment, levels = c("Pooled", "EUR 1", "EUR 2", "EUR 5"))

# get order of variables in plot right
itt_tc_all$outcome_label <- factor(
  itt_tc_all$outcome_label, 
  levels = 
    rev(outcome_labels_df$outcome_label)
    )
cace_tc_all$outcome_label <- factor(
  cace_tc_all$outcome_label, 
  levels = 
    rev(outcome_labels_df$outcome_label)
)


pdf(file="../figures/incentivization-effects-itt.pdf", height = 5, width = 7)
ggplot(data = itt_tc_all, aes(y = estimate, x = outcome_label)) + 
  geom_hline(yintercept = 0, colour = "black", lty = 2) + 
  geom_pointrange(aes(y = estimate, 
                      ymin = conf.low,
                      ymax = conf.high,
                      color = Treatment), 
                  position = position_dodge(width=-.5)) +
  scale_color_manual(values=c("darkgrey", "#99d8c9", "#41ae76", "#005824"))+
  ylim(-5, 5.2) + xlab("") + ylab("") +
  ggtitle("") + 
  coord_flip() +
  theme(legend.position="bottom",
        plot.margin = unit(c(0,0.2,0,0), "cm")) #t,r,b,l
dev.off()


# store results for combined effects plot
itt_tc_all_incent <- itt_tc_all



# combined experiments effects plot -------------------------

# combine results
itt_tc_all_message$experiment <- "Video message"
itt_tc_all_incent$experiment <- "Incentivization"
exp_results_all <- bind_rows(itt_tc_all_message, itt_tc_all_incent)

# get order of variables in plot right
exp_results_all$outcome_label <- factor(
  exp_results_all$outcome_label, 
  levels = 
    rev(c("Uptake (tracked)", "Uptake (hybrid)", "Uptake (reported)", "Knowledge", "Positive attitudes", "Message sharing (tracked)", "Information lookup (tracked)", "Bluetooth active (reported)", "Test/quarantine if given\nrisk warning (reported)", "Notify app if tested\npositive (reported)")
    ))
exp_results_all$Treatment <- factor(
  exp_results_all$Treatment, 
  levels = 
    c("Pooled", "Pro-social", "Self-interest", "EUR 1", "EUR 2", "EUR 5")
    )
exp_results_all$experiment <- factor(
  exp_results_all$experiment, 
  levels = 
    c("Video message", "Incentivization")
)

# plot
pdf(file="../figures/experiments-effects-itt.pdf", height = 7, width = 9)
ggplot(data = exp_results_all, aes(y = estimate, x = outcome_label)) + 
  geom_hline(yintercept = 0, colour = "black", lty = 2) + 
  geom_pointrange(aes(y = estimate, 
                      ymin = conf.low,
                      ymax = conf.high,
                      color = Treatment), 
                  position = position_dodge(width=-.5)) +
  facet_rep_wrap(~ experiment,  scales='free_x', repeat.tick.labels = "bottom", ncol = 2) +
  scale_color_manual(values=c("darkgrey", "blue","red", "#99d8c9", "#41ae76", "#005824")) +
  xlab("") + ylab("") +
  ggtitle("") + 
  coord_flip() + 
  theme(panel.background = element_blank(), 
        axis.title.y = element_blank(), 
        axis.title.x = element_text(hjust = 0.5, size = 8), 
        axis.text = element_text(size = 12), 
        plot.caption = element_text(size = 7), 
        strip.text.y = element_text(size = 12, colour = "black"),
        plot.margin=unit(c(0.1,0.1,0.1,0.1), "cm"),
        panel.spacing.x = unit(-5, "cm"),
        legend.position = "bottom",
        #legend.title = element_blank(),
        legend.key.size = unit(1, "line"),
        legend.box.background = element_rect(colour = "black"),
        panel.border = element_rect(colour = "black", fill = NA)) +
  guides(color = guide_legend(nrow = 1))
dev.off()




# plot ger
exp_results_all$experiment_ger <- ifelse(exp_results_all$experiment == "Video message", "Informationsvideo", "Incentivierung")
exp_results_all$Treatment <- c(rep("Kombiniert", 10), rep("Prosozial", 10), rep("Eigeninteresse", 10), rep("Kombiniert", 6), rep("EUR 1", 6), rep("EUR 2", 5), rep("EUR 5", 6))
exp_results_all$outcome_label_ger <- c(
  "Bluetooth aktiviert (berichtet)",
  "Installation (hybrid)",
  "Informationssuche",
  "App-Wissen",
  "Installation (berichtet)",
  "Informationsweitergabe",
  "Positives Testergebnis\nin App eintragen (berichtet)",
  "Nach Risikowarning\nTesten/Quarantäne (berichtet)",
  "Installation (getrackt)",
  "App-Einstellungen",
  "Bluetooth aktiviert (berichtet)",
  "Installation (hybrid)",
  "Informationssuche",
  "App-Wissen",
  "Installation (berichtet)",
  "Informationsweitergabe",
  "Positives Testergebnis\nin App eintragen (berichtet)",
  "Nach Risikowarning\nTesten/Quarantäne (berichtet)",
  "Installation (getrackt)",
  "App-Einstellungen",
  "Bluetooth aktiviert (berichtet)",
  "Installation (hybrid)",
  "Informationssuche",
  "App-Wissen",
  "Installation (berichtet)",
  "Informationsweitergabe",
  "Positives Testergebnis\nin App eintragen (berichtet)",
  "Nach Risikowarning\nTesten/Quarantäne (berichtet)",
  "Installation (getrackt)",
  "App-Einstellungen",
  "Installation (hybrid)",
  "App-Wissen",
  "Installation (berichtet)",
  "Informationsweitergabe",
  "Installation (getrackt)",
  "App-Einstellungen",
  "Installation (hybrid)",
  "App-Wissen",
  "Installation (berichtet)",
  "Informationsweitergabe",
  "Installation (getrackt)",
  "App-Einstellungen",
  "Installation (hybrid)",
  "App-Wissen",
  "Installation (berichtet)",
  "Installation (getrackt)",
  "App-Einstellungen",
  "Installation (hybrid)",
  "App-Wissen",
  "Installation (berichtet)",
  "Informationsweitergabe",
  "Installation (getrackt)",
  "App-Einstellungen"
)

# get order of variables in plot right
exp_results_all$outcome_label_ger_sorted <- factor(
  exp_results_all$outcome_label_ger, 
  levels = 
    rev(levels(as.factor(exp_results_all$outcome_label_ger))[c(7,8,6,2,1,5,4,3,9,10)]))
      
exp_results_all$Treatment <- factor(
  exp_results_all$Treatment, 
  levels = 
    c("Kombiniert", "Prosozial", "Eigeninteresse", "EUR 1", "EUR 2", "EUR 5")
)
exp_results_all$experiment_ger <- factor(
  exp_results_all$experiment_ger, 
  levels = 
    c("Informationsvideo", "Incentivierung")
)


pdf(file="../figures/experiments-effects-itt-ger.pdf", height = 7, width = 9)
ggplot(data = exp_results_all, aes(y = estimate, x = outcome_label_ger_sorted)) + 
  geom_hline(yintercept = 0, colour = "black", lty = 2) + 
  geom_pointrange(aes(y = estimate, 
                      ymin = conf.low,
                      ymax = conf.high,
                      color = Treatment), 
                  position = position_dodge(width=-.5)) +
  facet_rep_wrap(~ experiment_ger,  scales='free_x', repeat.tick.labels = "bottom", ncol = 2) +
  scale_color_manual(values=c("darkgrey", "blue","red", "#99d8c9", "#41ae76", "#005824")) +
  xlab("") + ylab("") +
  ggtitle("") + 
  coord_flip() + 
  theme(panel.background = element_blank(), 
        axis.title.y = element_blank(), 
        axis.title.x = element_text(hjust = 0.5, size = 8), 
        axis.text = element_text(size = 12), 
        plot.caption = element_text(size = 7), 
        strip.text.y = element_text(size = 12, colour = "black"),
        plot.margin=unit(c(0.1,0.1,0.1,0.1), "cm"),
        panel.spacing.x = unit(-5, "cm"),
        legend.position = "bottom",
        #legend.title = element_blank(),
        legend.key.size = unit(1, "line"),
        legend.box.background = element_rect(colour = "black"),
        panel.border = element_rect(colour = "black", fill = NA)) +
  guides(color = guide_legend(nrow = 1))
dev.off()





