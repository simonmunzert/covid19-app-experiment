### ------------------------------------------------------
### analysis of message intervention - figures
### ------------------------------------------------------


# define dv labels
outcome_labels_df <- data.frame(
  outcome = c("app_track_installed_before_w2_std", "app_hyb_installed_before_w2_std", "app_rep_installed_before_w2_std", "app_know_index_std.2", "att_scale_score_std.2", "app_sharing_clicks_std.1", "app_info_clicks_std.1", "app_bluetooth_act_std.2", "app_test_warning_std.2", "app_submit_positive_result_std.2"),
  outcome_label = c("Uptake (tracked)", "Uptake (hybrid)", "Uptake (reported)", "Knowledge", "Positive attitudes", "Message sharing (tracked)", "Information lookup (tracked)", "Bluetooth active (reported)", "Test/quarantine if given\nrisk warning (reported)", "Notify app if tested\npositive (reported)"))

outcome_labels_w3_df <- data.frame(
  outcome = c("app_track_installed_before_w3_std", "app_hyb_installed_before_w3_std", "app_rep_installed_before_w3_std", "app_know_index_std.3", "att_scale_score_std.3", "app_likely_install_std.3", "app_bluetooth_act_std.3", "app_test_warning_std.3", "app_submit_positive_result_std.3"),
  outcome_label = c("Uptake (tracked)", "Uptake (hybrid)", "Uptake (reported)", "Knowledge", "Positive attitudes", "Likelihood to install (reported)", "Bluetooth active (reported)", "Test/quarantine if given\nrisk warning (reported)", "Notify app if tested\npositive (reported)"))


### Coefplot for main results of message experiment -------------------

# tidy DIM results
dim_tc_pooled <- list(h_tc_1b_pooled$dim, h_tc_1c_pooled$dim, h_tc_2_pooled$dim, h_tc_3_pooled$dim, h_tc_5_pooled$dim, h_tc_6_pooled$dim, h_tc_7_pooled$dim, h_tc_8_pooled$dim, h_tc_lik_get_test_pooled$dim, h_tc_lik_rep_result_pooled$dim) %>% map(tidy) %>% bind_rows() %>% filter(term == "treat_any.1") %>% mutate(trt = "treat_any.1", Treatment = "Pooled") %>% merge(outcome_labels_df, by = "outcome")
dim_tc_prosoc <- list(h_tc_1b_prosoc$dim, h_tc_1c_prosoc$dim, h_tc_2_prosoc$dim, h_tc_3_prosoc$dim, h_tc_5_prosoc$dim, h_tc_6_prosoc$dim, h_tc_7_prosoc$dim, h_tc_8_prosoc$dim, h_tc_lik_get_test_prosoc$dim, h_tc_lik_rep_result_prosoc$dim) %>% map(tidy) %>% bind_rows() %>% filter(term == "treat_prosoc.1") %>% mutate(trt = "treat_prosoc.1", Treatment = "Pro-social") %>% merge(outcome_labels_df, by = "outcome")
dim_tc_selfint <- list(h_tc_1b_selfint$dim, h_tc_1c_selfint$dim, h_tc_2_selfint$dim, h_tc_3_selfint$dim, h_tc_5_selfint$dim, h_tc_6_selfint$dim, h_tc_7_selfint$dim, h_tc_8_selfint$dim, h_tc_lik_get_test_selfint$dim, h_tc_lik_rep_result_selfint$dim) %>% map(tidy) %>% bind_rows() %>% filter(term == "treat_selfint.1") %>% mutate(trt = "treat_selfint.1", Treatment = "Self-interest") %>% merge(outcome_labels_df, by = "outcome")

# tidy ITT results
itt_tc_pooled <- list(h_tc_1b_pooled$itt[[1]], h_tc_1c_pooled$itt[[1]], h_tc_2_pooled$itt[[1]], h_tc_3_pooled$itt[[1]], h_tc_5_pooled$itt[[1]], h_tc_6_pooled$itt[[1]], h_tc_7_pooled$itt[[1]], h_tc_8_pooled$itt[[1]], h_tc_lik_get_test_pooled$itt[[1]], h_tc_lik_rep_result_pooled$itt[[1]]) %>% map(tidy) %>% bind_rows() %>% filter(term == "treat_any.1TRUE") %>% mutate(trt = "treat_any.1", Treatment = "Pooled") %>% merge(outcome_labels_df, by = "outcome")
itt_tc_prosoc <- list(h_tc_1b_prosoc$itt[[1]], h_tc_1c_prosoc$itt[[1]], h_tc_2_prosoc$itt[[1]], h_tc_3_prosoc$itt[[1]], h_tc_5_prosoc$itt[[1]], h_tc_6_prosoc$itt[[1]], h_tc_7_prosoc$itt[[1]], h_tc_8_prosoc$itt[[1]], h_tc_lik_get_test_prosoc$itt[[1]], h_tc_lik_rep_result_prosoc$itt[[1]]) %>% map(tidy) %>% bind_rows() %>% filter(term == "treat_prosoc.1TRUE") %>% mutate(trt = "treat_prosoc.1", Treatment = "Pro-social") %>% merge(outcome_labels_df, by = "outcome")
itt_tc_selfint <- list(h_tc_1b_selfint$itt[[1]], h_tc_1c_selfint$itt[[1]], h_tc_2_selfint$itt[[1]], h_tc_3_selfint$itt[[1]], h_tc_5_selfint$itt[[1]], h_tc_6_selfint$itt[[1]], h_tc_7_selfint$itt[[1]], h_tc_8_selfint$itt[[1]], h_tc_lik_get_test_selfint$itt[[1]], h_tc_lik_rep_result_selfint$itt[[1]]) %>% map(tidy) %>% bind_rows() %>% filter(term == "treat_selfint.1TRUE") %>% mutate(trt = "treat_selfint.1", Treatment = "Self-interest") %>% merge(outcome_labels_df, by = "outcome")

# tidy CACE results
cace_tc_pooled <- list(h_tc_1b_pooled$cace, h_tc_1c_pooled$cace, h_tc_2_pooled$cace, h_tc_3_pooled$cace, h_tc_5_pooled$cace, h_tc_6_pooled$cace, h_tc_7_pooled$cace, h_tc_8_pooled$cace, h_tc_lik_get_test_pooled$cace, h_tc_lik_rep_result_pooled$cace) %>% map(tidy) %>% bind_rows() %>% filter(term == "treat_comp.1TRUE") %>% mutate(trt = "treat_any.1", Treatment = "Pooled") %>% merge(outcome_labels_df, by = "outcome")
cace_tc_prosoc <- list(h_tc_1b_prosoc$cace, h_tc_1c_prosoc$cace, h_tc_2_prosoc$cace, h_tc_3_prosoc$cace, h_tc_5_prosoc$cace, h_tc_6_prosoc$cace, h_tc_7_prosoc$cace, h_tc_8_prosoc$cace, h_tc_lik_get_test_prosoc$cace, h_tc_lik_rep_result_prosoc$cace) %>% map(tidy) %>% bind_rows() %>% filter(term == "treat_comp.1TRUE") %>% mutate(trt = "treat_prosoc.1", Treatment = "Pro-social") %>% merge(outcome_labels_df, by = "outcome")
cace_tc_selfint <- list(h_tc_1b_selfint$cace, h_tc_1c_selfint$cace, h_tc_2_selfint$cace, h_tc_3_selfint$cace, h_tc_5_selfint$cace, h_tc_6_selfint$cace, h_tc_7_selfint$cace, h_tc_8_selfint$cace, h_tc_lik_get_test_selfint$cace, h_tc_lik_rep_result_selfint$cace) %>% map(tidy) %>% bind_rows() %>% filter(term == "treat_comp.1TRUE") %>% mutate(trt = "treat_selfint.1", Treatment = "Self-interest") %>% merge(outcome_labels_df, by = "outcome")

cace_tc_all <- bind_rows(cace_tc_pooled, cace_tc_prosoc, cace_tc_selfint)
itt_tc_all <- bind_rows(itt_tc_pooled, itt_tc_prosoc, itt_tc_selfint)

# get order of variables in plot right
cace_tc_all$outcome_label <- factor(
  cace_tc_all$outcome_label, 
  levels = 
    rev(outcome_labels_df$outcome_label)
)
itt_tc_all$outcome_label <- factor(
  itt_tc_all$outcome_label, 
  levels = 
    rev(outcome_labels_df$outcome_label)
)
                                    

pdf(file="../figures/message-effects-cace.pdf", height = 5, width = 7)
theme_set(theme_bw())
ggplot(data = cace_tc_all, aes(y = estimate, x = outcome_label)) + 
  geom_hline(yintercept = 0, colour = "black", lty = 2) + 
  geom_pointrange(aes(y = estimate, 
                      ymin = conf.low,
                      ymax = conf.high,
                      color = Treatment), 
                  position = position_dodge(width=-.5)) +
  scale_color_manual(values=c("darkgrey", "blue","red"))+
  ylim(-.2, .5) + xlab("") + ylab("") +
  ggtitle("") + 
  coord_flip() +
  theme(legend.position="bottom",
        plot.margin = unit(c(0,0.2,0,0), "cm")) #t,r,b,l
dev.off()


pdf(file="../figures/message-effects-itt.pdf", height = 5, width = 7)
theme_set(theme_bw())
ggplot(data = itt_tc_all, aes(y = estimate, x = outcome_label)) + 
  geom_hline(yintercept = 0, colour = "black", lty = 2) + 
  geom_pointrange(aes(y = estimate, 
                      ymin = conf.low,
                      ymax = conf.high,
                      color = Treatment), 
                  position = position_dodge(width=-.5)) +
  scale_color_manual(values=c("darkgrey", "blue","red"))+
  ylim(-.2, .5) + xlab("") + ylab("") +
  ggtitle("") + 
  coord_flip() +
  theme(legend.position="bottom",
        plot.margin = unit(c(0,0.2,0,0), "cm")) #t,r,b,l
dev.off()


# store results for combined effect plot
cace_tc_all_message <- cace_tc_all
itt_tc_all_message <- itt_tc_all


### Coefplot for main results of message experiment; subset of those who passed manipulation check -------------------

# tidy DIM results
dim_tc_pooled <- list(h_tc_1b_pooled_check$dim, h_tc_1c_pooled_check$dim, h_tc_2_pooled_check$dim, h_tc_3_pooled_check$dim, h_tc_5_pooled_check$dim, h_tc_6_pooled_check$dim, h_tc_7_pooled_check$dim, h_tc_8_pooled_check$dim, h_tc_lik_get_test_pooled_check$dim, h_tc_lik_rep_result_pooled_check$dim) %>% map(tidy) %>% bind_rows() %>% filter(term == "treat_any.1") %>% mutate(trt = "treat_any.1", Treatment = "Pooled") %>% merge(outcome_labels_df, by = "outcome")
dim_tc_prosoc <- list(h_tc_1b_prosoc_check$dim, h_tc_1c_prosoc_check$dim, h_tc_2_prosoc_check$dim, h_tc_3_prosoc_check$dim, h_tc_5_prosoc_check$dim, h_tc_6_prosoc_check$dim, h_tc_7_prosoc_check$dim, h_tc_8_prosoc_check$dim, h_tc_lik_get_test_prosoc_check$dim, h_tc_lik_rep_result_prosoc_check$dim) %>% map(tidy) %>% bind_rows() %>% filter(term == "treat_prosoc.1") %>% mutate(trt = "treat_prosoc.1", Treatment = "Pro-social") %>% merge(outcome_labels_df, by = "outcome")
dim_tc_selfint <- list(h_tc_1b_selfint_check$dim, h_tc_1c_selfint_check$dim, h_tc_2_selfint_check$dim, h_tc_3_selfint_check$dim, h_tc_5_selfint_check$dim, h_tc_6_selfint_check$dim, h_tc_7_selfint_check$dim, h_tc_8_selfint_check$dim, h_tc_lik_get_test_selfint_check$dim, h_tc_lik_rep_result_selfint_check$dim) %>% map(tidy) %>% bind_rows() %>% filter(term == "treat_selfint.1") %>% mutate(trt = "treat_selfint.1", Treatment = "Self-interest") %>% merge(outcome_labels_df, by = "outcome")

# tidy ITT results
itt_tc_pooled <- list(h_tc_1b_pooled_check$itt[[1]], h_tc_1c_pooled_check$itt[[1]], h_tc_2_pooled_check$itt[[1]], h_tc_3_pooled_check$itt[[1]], h_tc_5_pooled_check$itt[[1]], h_tc_6_pooled_check$itt[[1]], h_tc_7_pooled_check$itt[[1]], h_tc_8_pooled_check$itt[[1]], h_tc_lik_get_test_pooled_check$itt[[1]], h_tc_lik_rep_result_pooled_check$itt[[1]]) %>% map(tidy) %>% bind_rows() %>% filter(term == "treat_any.1TRUE") %>% mutate(trt = "treat_any.1", Treatment = "Pooled") %>% merge(outcome_labels_df, by = "outcome")
itt_tc_prosoc <- list(h_tc_1b_prosoc_check$itt[[1]], h_tc_1c_prosoc_check$itt[[1]], h_tc_2_prosoc_check$itt[[1]], h_tc_3_prosoc_check$itt[[1]], h_tc_5_prosoc_check$itt[[1]], h_tc_6_prosoc_check$itt[[1]], h_tc_7_prosoc_check$itt[[1]], h_tc_8_prosoc_check$itt[[1]], h_tc_lik_get_test_prosoc_check$itt[[1]], h_tc_lik_rep_result_prosoc_check$itt[[1]]) %>% map(tidy) %>% bind_rows() %>% filter(term == "treat_prosoc.1TRUE") %>% mutate(trt = "treat_prosoc.1", Treatment = "Pro-social") %>% merge(outcome_labels_df, by = "outcome")
itt_tc_selfint <- list(h_tc_1b_selfint_check$itt[[1]], h_tc_1c_selfint_check$itt[[1]], h_tc_2_selfint_check$itt[[1]], h_tc_3_selfint_check$itt[[1]], h_tc_5_selfint_check$itt[[1]], h_tc_6_selfint_check$itt[[1]], h_tc_7_selfint_check$itt[[1]], h_tc_8_selfint_check$itt[[1]], h_tc_lik_get_test_selfint_check$itt[[1]], h_tc_lik_rep_result_selfint_check$itt[[1]]) %>% map(tidy) %>% bind_rows() %>% filter(term == "treat_selfint.1TRUE") %>% mutate(trt = "treat_selfint.1", Treatment = "Self-interest") %>% merge(outcome_labels_df, by = "outcome")

# tidy CACE results
cace_tc_pooled <- list(h_tc_1b_pooled_check$cace, h_tc_1c_pooled_check$cace, h_tc_2_pooled_check$cace, h_tc_3_pooled_check$cace, h_tc_5_pooled_check$cace, h_tc_6_pooled_check$cace, h_tc_7_pooled_check$cace, h_tc_8_pooled_check$cace, h_tc_lik_get_test_pooled_check$cace, h_tc_lik_rep_result_pooled_check$cace) %>% map(tidy) %>% bind_rows() %>% filter(term == "treat_comp.1TRUE") %>% mutate(trt = "treat_any.1", Treatment = "Pooled") %>% merge(outcome_labels_df, by = "outcome")
cace_tc_prosoc <- list(h_tc_1b_prosoc_check$cace, h_tc_1c_prosoc_check$cace, h_tc_2_prosoc_check$cace, h_tc_3_prosoc_check$cace, h_tc_5_prosoc_check$cace, h_tc_6_prosoc_check$cace, h_tc_7_prosoc_check$cace, h_tc_8_prosoc_check$cace, h_tc_lik_get_test_prosoc_check$cace, h_tc_lik_rep_result_prosoc_check$cace) %>% map(tidy) %>% bind_rows() %>% filter(term == "treat_comp.1TRUE") %>% mutate(trt = "treat_prosoc.1", Treatment = "Pro-social") %>% merge(outcome_labels_df, by = "outcome")
cace_tc_selfint <- list(h_tc_1b_selfint_check$cace, h_tc_1c_selfint_check$cace, h_tc_2_selfint_check$cace, h_tc_3_selfint_check$cace, h_tc_5_selfint_check$cace, h_tc_6_selfint_check$cace, h_tc_7_selfint_check$cace, h_tc_8_selfint_check$cace, h_tc_lik_get_test_selfint_check$cace, h_tc_lik_rep_result_selfint_check$cace) %>% map(tidy) %>% bind_rows() %>% filter(term == "treat_comp.1TRUE") %>% mutate(trt = "treat_selfint.1", Treatment = "Self-interest") %>% merge(outcome_labels_df, by = "outcome")

cace_tc_all <- bind_rows(cace_tc_pooled, cace_tc_prosoc, cace_tc_selfint)
itt_tc_all <- bind_rows(itt_tc_pooled, itt_tc_prosoc, itt_tc_selfint)

# get order of variables in plot right
cace_tc_all$outcome_label <- factor(
  cace_tc_all$outcome_label, 
  levels = 
    rev(outcome_labels_df$outcome_label)
)
itt_tc_all$outcome_label <- factor(
  itt_tc_all$outcome_label, 
  levels = 
    rev(outcome_labels_df$outcome_label)
)


pdf(file="../figures/message-effects-cace-check.pdf", height = 5, width = 7)
theme_set(theme_bw())
ggplot(data = cace_tc_all, aes(y = estimate, x = outcome_label)) + 
  geom_hline(yintercept = 0, colour = "black", lty = 2) + 
  geom_pointrange(aes(y = estimate, 
                      ymin = conf.low,
                      ymax = conf.high,
                      color = Treatment), 
                  position = position_dodge(width=-.5)) +
  scale_color_manual(values=c("darkgrey", "blue","red"))+
  ylim(-.2, .7) + xlab("") + ylab("") +
  ggtitle("") + 
  coord_flip() +
  theme(legend.position="bottom",
        plot.margin = unit(c(0,0.2,0,0), "cm")) #t,r,b,l
dev.off()

pdf(file="../figures/message-effects-itt-check.pdf", height = 5, width = 7)
theme_set(theme_bw())
ggplot(data = itt_tc_all, aes(y = estimate, x = outcome_label)) + 
  geom_hline(yintercept = 0, colour = "black", lty = 2) + 
  geom_pointrange(aes(y = estimate, 
                      ymin = conf.low,
                      ymax = conf.high,
                      color = Treatment), 
                  position = position_dodge(width=-.5)) +
  scale_color_manual(values=c("darkgrey", "blue","red"))+
  ylim(-.2, .7) + xlab("") + ylab("") +
  ggtitle("") + 
  coord_flip() +
  theme(legend.position="bottom",
        plot.margin = unit(c(0,0.2,0,0), "cm")) #t,r,b,l
dev.off()




### Coefplot for results of message experiment, W3 outcomes -------------------

# tidy DIM results
dim_tc_pooled <- list(h_tc_1b_pooled_w3$dim, h_tc_1c_pooled_w3$dim, h_tc_2_pooled_w3$dim, h_tc_3_pooled_w3$dim, h_tc_4_pooled_w3$dim, h_tc_5_pooled_w3$dim, h_tc_6_pooled_w3$dim, h_tc_lik_get_test_pooled_w3$dim, h_tc_lik_rep_result_pooled_w3$dim) %>% map(tidy) %>% bind_rows() %>% filter(term == "treat_any.1") %>% mutate(trt = "treat_any.1", Treatment = "Pooled") %>% merge(outcome_labels_w3_df, by = "outcome")
dim_tc_prosoc <- list(h_tc_1b_prosoc_w3$dim, h_tc_1c_prosoc_w3$dim, h_tc_2_prosoc_w3$dim, h_tc_3_prosoc_w3$dim, h_tc_4_prosoc_w3$dim, h_tc_5_prosoc_w3$dim, h_tc_6_prosoc_w3$dim, h_tc_lik_get_test_prosoc_w3$dim, h_tc_lik_rep_result_prosoc_w3$dim) %>% map(tidy) %>% bind_rows() %>% filter(term == "treat_prosoc.1") %>% mutate(trt = "treat_prosoc.1", Treatment = "Pro-social") %>% merge(outcome_labels_w3_df, by = "outcome")
dim_tc_selfint <- list(h_tc_1b_selfint_w3$dim, h_tc_1c_selfint_w3$dim, h_tc_2_selfint_w3$dim, h_tc_3_selfint_w3$dim, h_tc_4_selfint_w3$dim, h_tc_5_selfint_w3$dim, h_tc_6_selfint_w3$dim, h_tc_lik_get_test_selfint_w3$dim, h_tc_lik_rep_result_selfint_w3$dim) %>% map(tidy) %>% bind_rows() %>% filter(term == "treat_selfint.1") %>% mutate(trt = "treat_selfint.1", Treatment = "Self-interest") %>% merge(outcome_labels_w3_df, by = "outcome")

# tidy ITT results
itt_tc_pooled <- list(h_tc_1b_pooled_w3$itt[[1]], h_tc_1c_pooled_w3$itt[[1]], h_tc_2_pooled_w3$itt[[1]], h_tc_3_pooled_w3$itt[[1]], h_tc_4_pooled_w3$itt[[1]], h_tc_5_pooled_w3$itt[[1]], h_tc_6_pooled_w3$itt[[1]], h_tc_lik_get_test_pooled_w3$itt[[1]], h_tc_lik_rep_result_pooled_w3$itt[[1]]) %>% map(tidy) %>% bind_rows() %>% filter(term == "treat_any.1TRUE") %>% mutate(trt = "treat_any.1", Treatment = "Pooled") %>% merge(outcome_labels_w3_df, by = "outcome")
itt_tc_prosoc <- list(h_tc_1b_prosoc_w3$itt[[1]], h_tc_1c_prosoc_w3$itt[[1]], h_tc_2_prosoc_w3$itt[[1]], h_tc_3_prosoc_w3$itt[[1]], h_tc_4_prosoc_w3$itt[[1]], h_tc_5_prosoc_w3$itt[[1]], h_tc_6_prosoc_w3$itt[[1]], h_tc_lik_get_test_prosoc_w3$itt[[1]], h_tc_lik_rep_result_prosoc_w3$itt[[1]]) %>% map(tidy) %>% bind_rows() %>% filter(term == "treat_prosoc.1TRUE") %>% mutate(trt = "treat_prosoc.1", Treatment = "Pro-social") %>% merge(outcome_labels_w3_df, by = "outcome")
itt_tc_selfint <- list(h_tc_1b_selfint_w3$itt[[1]], h_tc_1c_selfint_w3$itt[[1]], h_tc_2_selfint_w3$itt[[1]], h_tc_3_selfint_w3$itt[[1]], h_tc_4_selfint_w3$itt[[1]], h_tc_5_selfint_w3$itt[[1]], h_tc_6_selfint_w3$itt[[1]], h_tc_lik_get_test_selfint_w3$itt[[1]], h_tc_lik_rep_result_selfint_w3$itt[[1]]) %>% map(tidy) %>% bind_rows() %>% filter(term == "treat_selfint.1TRUE") %>% mutate(trt = "treat_selfint.1", Treatment = "Self-interest") %>% merge(outcome_labels_w3_df, by = "outcome")

# tidy CACE results
cace_tc_pooled <- list(h_tc_1b_pooled_w3$cace, h_tc_1c_pooled_w3$cace, h_tc_2_pooled_w3$cace, h_tc_3_pooled_w3$cace, h_tc_4_pooled_w3$cace, h_tc_5_pooled_w3$cace, h_tc_6_pooled_w3$cace, h_tc_lik_get_test_pooled_w3$cace, h_tc_lik_rep_result_pooled_w3$cace) %>% map(tidy) %>% bind_rows() %>% filter(term == "treat_comp.1TRUE") %>% mutate(trt = "treat_any.1", Treatment = "Pooled") %>% merge(outcome_labels_w3_df, by = "outcome")
cace_tc_prosoc <- list(h_tc_1b_prosoc_w3$cace, h_tc_1c_prosoc_w3$cace, h_tc_2_prosoc_w3$cace, h_tc_3_prosoc_w3$cace, h_tc_4_prosoc_w3$cace, h_tc_5_prosoc_w3$cace, h_tc_6_prosoc_w3$cace, h_tc_lik_get_test_prosoc_w3$cace, h_tc_lik_rep_result_prosoc_w3$cace) %>% map(tidy) %>% bind_rows() %>% filter(term == "treat_comp.1TRUE") %>% mutate(trt = "treat_prosoc.1", Treatment = "Pro-social") %>% merge(outcome_labels_w3_df, by = "outcome")
cace_tc_selfint <- list(h_tc_1b_selfint_w3$cace, h_tc_1c_selfint_w3$cace, h_tc_2_selfint_w3$cace, h_tc_3_selfint_w3$cace, h_tc_4_selfint_w3$cace, h_tc_5_selfint_w3$cace, h_tc_6_selfint_w3$cace, h_tc_lik_get_test_selfint_w3$cace, h_tc_lik_rep_result_selfint_w3$cace) %>% map(tidy) %>% bind_rows() %>% filter(term == "treat_comp.1TRUE") %>% mutate(trt = "treat_selfint.1", Treatment = "Self-interest") %>% merge(outcome_labels_w3_df, by = "outcome")

cace_tc_all <- bind_rows(cace_tc_pooled, cace_tc_prosoc, cace_tc_selfint)
itt_tc_all <- bind_rows(itt_tc_pooled, itt_tc_prosoc, itt_tc_selfint)

# get order of variables in plot right
cace_tc_all$outcome_label <- factor(
  cace_tc_all$outcome_label, 
  levels = 
    rev(outcome_labels_w3_df$outcome_label)
)
itt_tc_all$outcome_label <- factor(
  itt_tc_all$outcome_label, 
  levels = 
    rev(outcome_labels_w3_df$outcome_label)
)


pdf(file="../figures/message-effects-w3-cace.pdf", height = 5, width = 7)
theme_set(theme_bw())
ggplot(data = cace_tc_all, aes(y = estimate, x = outcome_label)) + 
  geom_hline(yintercept = 0, colour = "black", lty = 2) + 
  geom_pointrange(aes(y = estimate, 
                      ymin = conf.low,
                      ymax = conf.high,
                      color = Treatment), 
                  position = position_dodge(width=-.5)) +
  scale_color_manual(values=c("darkgrey", "blue","red"))+
  ylim(-.4, .6) + xlab("") + ylab("") +
  ggtitle("") + 
  coord_flip() +
  theme(legend.position="bottom",
        plot.margin = unit(c(0,0.2,0,0), "cm")) #t,r,b,l
dev.off()


pdf(file="../figures/message-effects-w3-itt.pdf", height = 5, width = 7)
theme_set(theme_bw())
ggplot(data = itt_tc_all, aes(y = estimate, x = outcome_label)) + 
  geom_hline(yintercept = 0, colour = "black", lty = 2) + 
  geom_pointrange(aes(y = estimate, 
                      ymin = conf.low,
                      ymax = conf.high,
                      color = Treatment), 
                  position = position_dodge(width=-.5)) +
  scale_color_manual(values=c("darkgrey", "blue","red"))+
  ylim(-.4, .6) + xlab("") + ylab("") +
  ggtitle("") + 
  coord_flip() +
  theme(legend.position="bottom",
        plot.margin = unit(c(0,0.2,0,0), "cm")) #t,r,b,l
dev.off()



### Coefplot for results of message experiment, W3 outcomes ; subset of those who passed manipulation check -------------------

# tidy DIM results
dim_tc_pooled <- list(h_tc_1b_pooled_w3_check$dim, h_tc_1c_pooled_w3_check$dim, h_tc_2_pooled_w3_check$dim, h_tc_3_pooled_w3_check$dim, h_tc_4_pooled_w3_check$dim, h_tc_5_pooled_w3_check$dim, h_tc_6_pooled_w3_check$dim, h_tc_lik_get_test_pooled_w3_check$dim, h_tc_lik_rep_result_pooled_w3_check$dim) %>% map(tidy) %>% bind_rows() %>% filter(term == "treat_any.1") %>% mutate(trt = "treat_any.1", Treatment = "Pooled") %>% merge(outcome_labels_w3_df, by = "outcome")
dim_tc_prosoc <- list(h_tc_1b_prosoc_w3_check$dim, h_tc_1c_prosoc_w3_check$dim, h_tc_2_prosoc_w3_check$dim, h_tc_3_prosoc_w3_check$dim, h_tc_4_prosoc_w3_check$dim, h_tc_5_prosoc_w3_check$dim, h_tc_6_prosoc_w3_check$dim, h_tc_lik_get_test_prosoc_w3_check$dim, h_tc_lik_rep_result_prosoc_w3_check$dim) %>% map(tidy) %>% bind_rows() %>% filter(term == "treat_prosoc.1") %>% mutate(trt = "treat_prosoc.1", Treatment = "Pro-social") %>% merge(outcome_labels_w3_df, by = "outcome")
dim_tc_selfint <- list(h_tc_1b_selfint_w3_check$dim, h_tc_1c_selfint_w3_check$dim, h_tc_2_selfint_w3_check$dim, h_tc_3_selfint_w3_check$dim, h_tc_4_selfint_w3_check$dim, h_tc_5_selfint_w3_check$dim, h_tc_6_selfint_w3_check$dim, h_tc_lik_get_test_selfint_w3_check$dim, h_tc_lik_rep_result_selfint_w3_check$dim) %>% map(tidy) %>% bind_rows() %>% filter(term == "treat_selfint.1") %>% mutate(trt = "treat_selfint.1", Treatment = "Self-interest") %>% merge(outcome_labels_w3_df, by = "outcome")

# tidy ITT results
itt_tc_pooled <- list(h_tc_1b_pooled_w3_check$itt[[1]], h_tc_1c_pooled_w3_check$itt[[1]], h_tc_2_pooled_w3_check$itt[[1]], h_tc_3_pooled_w3_check$itt[[1]], h_tc_4_pooled_w3_check$itt[[1]], h_tc_5_pooled_w3_check$itt[[1]], h_tc_6_pooled_w3_check$itt[[1]], h_tc_lik_get_test_pooled_w3_check$itt[[1]], h_tc_lik_rep_result_pooled_w3_check$itt[[1]]) %>% map(tidy) %>% bind_rows() %>% filter(term == "treat_any.1TRUE") %>% mutate(trt = "treat_any.1", Treatment = "Pooled") %>% merge(outcome_labels_w3_df, by = "outcome")
itt_tc_prosoc <- list(h_tc_1b_prosoc_w3_check$itt[[1]], h_tc_1c_prosoc_w3_check$itt[[1]], h_tc_2_prosoc_w3_check$itt[[1]], h_tc_3_prosoc_w3_check$itt[[1]], h_tc_4_prosoc_w3_check$itt[[1]], h_tc_5_prosoc_w3_check$itt[[1]], h_tc_6_prosoc_w3_check$itt[[1]], h_tc_lik_get_test_prosoc_w3_check$itt[[1]], h_tc_lik_rep_result_prosoc_w3_check$itt[[1]]) %>% map(tidy) %>% bind_rows() %>% filter(term == "treat_prosoc.1TRUE") %>% mutate(trt = "treat_prosoc.1", Treatment = "Pro-social") %>% merge(outcome_labels_w3_df, by = "outcome")
itt_tc_selfint <- list(h_tc_1b_selfint_w3_check$itt[[1]], h_tc_1c_selfint_w3_check$itt[[1]], h_tc_2_selfint_w3_check$itt[[1]], h_tc_3_selfint_w3_check$itt[[1]], h_tc_4_selfint_w3_check$itt[[1]], h_tc_5_selfint_w3_check$itt[[1]], h_tc_6_selfint_w3_check$itt[[1]], h_tc_lik_get_test_selfint_w3_check$itt[[1]], h_tc_lik_rep_result_selfint_w3_check$itt[[1]]) %>% map(tidy) %>% bind_rows() %>% filter(term == "treat_selfint.1TRUE") %>% mutate(trt = "treat_selfint.1", Treatment = "Self-interest") %>% merge(outcome_labels_w3_df, by = "outcome")

# tidy CACE results
cace_tc_pooled <- list(h_tc_1b_pooled_w3_check$cace, h_tc_1c_pooled_w3_check$cace, h_tc_2_pooled_w3_check$cace, h_tc_3_pooled_w3_check$cace, h_tc_4_pooled_w3_check$cace, h_tc_5_pooled_w3_check$cace, h_tc_6_pooled_w3_check$cace, h_tc_lik_get_test_pooled_w3_check$cace, h_tc_lik_rep_result_pooled_w3_check$cace) %>% map(tidy) %>% bind_rows() %>% filter(term == "treat_comp.1TRUE") %>% mutate(trt = "treat_any.1", Treatment = "Pooled") %>% merge(outcome_labels_w3_df, by = "outcome")
cace_tc_prosoc <- list(h_tc_1b_prosoc_w3_check$cace, h_tc_1c_prosoc_w3_check$cace, h_tc_2_prosoc_w3_check$cace, h_tc_3_prosoc_w3_check$cace, h_tc_4_prosoc_w3_check$cace, h_tc_5_prosoc_w3_check$cace, h_tc_6_prosoc_w3_check$cace, h_tc_lik_get_test_prosoc_w3_check$cace, h_tc_lik_rep_result_prosoc_w3_check$cace) %>% map(tidy) %>% bind_rows() %>% filter(term == "treat_comp.1TRUE") %>% mutate(trt = "treat_prosoc.1", Treatment = "Pro-social") %>% merge(outcome_labels_w3_df, by = "outcome")
cace_tc_selfint <- list(h_tc_1b_selfint_w3_check$cace, h_tc_1c_selfint_w3_check$cace, h_tc_2_selfint_w3_check$cace, h_tc_3_selfint_w3_check$cace, h_tc_4_selfint_w3_check$cace, h_tc_5_selfint_w3_check$cace, h_tc_6_selfint_w3_check$cace, h_tc_lik_get_test_selfint_w3_check$cace, h_tc_lik_rep_result_selfint_w3_check$cace) %>% map(tidy) %>% bind_rows() %>% filter(term == "treat_comp.1TRUE") %>% mutate(trt = "treat_selfint.1", Treatment = "Self-interest") %>% merge(outcome_labels_w3_df, by = "outcome")

cace_tc_all <- bind_rows(cace_tc_pooled, cace_tc_prosoc, cace_tc_selfint)
itt_tc_all <- bind_rows(itt_tc_pooled, itt_tc_prosoc, itt_tc_selfint)

# get order of variables in plot right
cace_tc_all$outcome_label <- factor(
  cace_tc_all$outcome_label, 
  levels = 
    rev(outcome_labels_w3_df$outcome_label)
)
itt_tc_all$outcome_label <- factor(
  itt_tc_all$outcome_label, 
  levels = 
    rev(outcome_labels_w3_df$outcome_label)
)


pdf(file="../figures/message-effects-w3-check-cace.pdf", height = 5, width = 7)
ggplot(data = cace_tc_all, aes(y = estimate, x = outcome_label)) + 
  geom_hline(yintercept = 0, colour = "black", lty = 2) + 
  geom_pointrange(aes(y = estimate, 
                      ymin = conf.low,
                      ymax = conf.high,
                      color = Treatment), 
                  position = position_dodge(width=-.5)) +
  scale_color_manual(values=c("darkgrey", "blue","red"))+
  ylim(-.3, .8) + xlab("") + ylab("") +
  ggtitle("") + 
  coord_flip() +
  theme(legend.position="bottom",
        plot.margin = unit(c(0,0.2,0,0), "cm")) #t,r,b,l
dev.off()


pdf(file="../figures/message-effects-w3-check-itt.pdf", height = 5, width = 7)
ggplot(data = itt_tc_all, aes(y = estimate, x = outcome_label)) + 
  geom_hline(yintercept = 0, colour = "black", lty = 2) + 
  geom_pointrange(aes(y = estimate, 
                      ymin = conf.low,
                      ymax = conf.high,
                      color = Treatment), 
                  position = position_dodge(width=-.5)) +
  scale_color_manual(values=c("darkgrey", "blue","red"))+
  ylim(-.3, .8) + xlab("") + ylab("") +
  ggtitle("") + 
  coord_flip() +
  theme(legend.position="bottom",
        plot.margin = unit(c(0,0.2,0,0), "cm")) #t,r,b,l
dev.off()


