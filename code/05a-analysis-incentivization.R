### -----------------------------
### analysis of app install incentivization
### -----------------------------


# load data --------------------------

ger_df_wide <- readRDS("../data/clean/ger_df_wide_an.RDS")



### Hypothesis on incentivization acceptance ---------------

### H1: Subjects offered higher monetary incentives will be more likely to be willing to install the app than subjects assigned to lower monetary incentives.

# Tested in incentivization-agreement-barchart.pdf


### Hypotheses on Uptake --------------------------------

### H2a: Subjects in any of the incentivization groups will be more likely to be observed to install the app than subjects assigned to the control group.
### H2b: The differences between incentivized subjects and the control group referred to in H2a will be larger for subjects offered higher monetary incentives.

# define vars
dv <- "app_track_installed_at_w3_incent_std"
dv_pre <- "app_track_installed_at_w2"
dv_label <- "Uptake (tracked)"
dat_surveytrackingplus <- filter(ger_df_wide, sample_type.1 == "surveytracking" | (sample_type.1 == "trackingonly" & app_track_installed_at_w2 == FALSE))


# run models
h_incent_2a_pooled <- run_models(trt = "treat_incent_any.2", dv = dv, dv_pre = dv_pre, covars = covars_red, data = filter(dat_surveytrackingplus, !is.na(treat_incent_any.2)), run.cace = FALSE)
# with non-standardized variable to get estimate of percentage point increase for main text
run_models(trt = "treat_incent_any.2", dv = "app_track_installed_at_w3", dv_pre = dv_pre, covars = covars_red, data = filter(dat_surveytrackingplus, !is.na(treat_incent_any.2)), run.cace = FALSE)
h_incent_2b_eur1 <- run_models(trt = "treat_incent_any.2", dv = dv, dv_pre = dv_pre, covars = covars_red, data = filter(dat_surveytrackingplus, !is.na(treat_incent_any.2), !(treat_incent.2 %in% c("EUR 2", "EUR 5"))), run.cace = FALSE)
h_incent_2b_eur2 <- run_models(trt = "treat_incent_any.2", dv = dv, dv_pre = dv_pre, covars = covars_red, data = filter(dat_surveytrackingplus, !is.na(treat_incent_any.2), !(treat_incent.2 %in% c("EUR 1", "EUR 5"))), run.cace = FALSE)
h_incent_2b_eur5 <- run_models(trt = "treat_incent_any.2", dv = dv, dv_pre = dv_pre, covars = covars_red, data = filter(dat_surveytrackingplus, !is.na(treat_incent_any.2), !(treat_incent.2 %in% c("EUR 1", "EUR 2"))), run.cace = FALSE)
h_incent_2b_eur1_eur_2 <- run_models(trt = "treat_incent_eur2.2", dv = dv, dv_pre = dv_pre, covars = covars_red, data = filter(dat_surveytrackingplus, !is.na(treat_incent_any.2), !(treat_incent.2 %in% c("EUR 5", "Control"))), run.cace = FALSE)
h_incent_2b_eur1_eur_5 <- run_models(trt = "treat_incent_eur5.2", dv = dv, dv_pre = dv_pre, covars = covars_red, data = filter(dat_surveytrackingplus, !is.na(treat_incent_any.2), !(treat_incent.2 %in% c("EUR 2", "Control"))), run.cace = FALSE)
h_incent_2b_eur2_eur_5 <- run_models(trt = "treat_incent_eur5.2", dv = dv, dv_pre = dv_pre, covars = covars_red, data = filter(dat_surveytrackingplus, !is.na(treat_incent_any.2), !(treat_incent.2 %in% c("EUR 1", "Control"))), run.cace = FALSE)

# export results tables
format_latex(h_incent_2a_pooled, dv_label, trt_label = "Pooled vs. control", path = "../figures/h_incent_2a_pooled.tex", add.cace = FALSE)
format_latex(h_incent_2b_eur1, dv_label, trt_label = "EUR 1 vs. control", path = "../figures/h_incent_2b_eur1.tex", add.cace = FALSE)
format_latex(h_incent_2b_eur2, dv_label, trt_label = "EUR 2 vs. control", path = "../figures/h_incent_2b_eur2.tex", add.cace = FALSE)
format_latex(h_incent_2b_eur5, dv_label, trt_label = "EUR 5 vs. control", path = "../figures/h_incent_2b_eur5.tex", add.cace = FALSE)
format_latex(h_incent_2b_eur1_eur_2, dv_label, trt_label = "EUR 2 vs. EUR 1", path = "../figures/h_incent_2b_eur1_eur_2.tex", add.cace = FALSE)
format_latex(h_incent_2b_eur1_eur_5, dv_label, trt_label = "EUR 5 vs. EUR 1", path = "../figures/h_incent_2b_eur1_eur_5.tex", add.cace = FALSE)
format_latex(h_incent_2b_eur2_eur_5, dv_label, trt_label = "EUR 5 vs. EUR 2", path = "../figures/h_incent_2b_eur2_eur_5.tex", add.cace = FALSE)





### Hybrid outcome

# define vars
dv <- "app_hyb_installed_at_w3_incent_std"
dv_pre <- "app_hyb_installed_at_w2"
dv_label <- "Uptake (hybrid)"
dat_surveytrackingplus <- filter(ger_df_wide, sample_type.1 == "surveytracking" | sample_type.1 == "surveyonly" |(sample_type.1 == "trackingonly" & app_track_installed_at_w2 == FALSE))


# run models
h_incent_2ah_pooled <- run_models(trt = "treat_incent_any.2", dv = dv, dv_pre = dv_pre, covars = covars_red, data = filter(dat_surveytrackingplus, !is.na(treat_incent_any.2)), run.cace = FALSE)
h_incent_2bh_eur1 <- run_models(trt = "treat_incent_any.2", dv = dv, dv_pre = dv_pre, covars = covars_red, data = filter(dat_surveytrackingplus, !is.na(treat_incent_any.2), !(treat_incent.2 %in% c("EUR 2", "EUR 5"))), run.cace = FALSE)
h_incent_2bh_eur2 <- run_models(trt = "treat_incent_any.2", dv = dv, dv_pre = dv_pre, covars = covars_red, data = filter(dat_surveytrackingplus, !is.na(treat_incent_any.2), !(treat_incent.2 %in% c("EUR 1", "EUR 5"))), run.cace = FALSE)
h_incent_2bh_eur5 <- run_models(trt = "treat_incent_any.2", dv = dv, dv_pre = dv_pre, covars = covars_red, data = filter(dat_surveytrackingplus, !is.na(treat_incent_any.2), !(treat_incent.2 %in% c("EUR 1", "EUR 2"))), run.cace = FALSE)
h_incent_2bh_eur1_eur_2 <- run_models(trt = "treat_incent_eur2.2", dv = dv, dv_pre = dv_pre, covars = covars_red, data = filter(dat_surveytrackingplus, !is.na(treat_incent_any.2), !(treat_incent.2 %in% c("EUR 5", "Control"))), run.cace = FALSE)
h_incent_2bh_eur1_eur_5 <- run_models(trt = "treat_incent_eur5.2", dv = dv, dv_pre = dv_pre, covars = covars_red, data = filter(dat_surveytrackingplus, !is.na(treat_incent_any.2), !(treat_incent.2 %in% c("EUR 2", "Control"))), run.cace = FALSE)
h_incent_2bh_eur2_eur_5 <- run_models(trt = "treat_incent_eur5.2", dv = dv, dv_pre = dv_pre, covars = covars_red, data = filter(dat_surveytrackingplus, !is.na(treat_incent_any.2), !(treat_incent.2 %in% c("EUR 1", "Control"))), run.cace = FALSE)

# export results tables
format_latex(h_incent_2ah_pooled, dv_label, trt_label = "Pooled vs. control", path = "../figures/h_incent_2ah_pooled.tex", add.cace = FALSE)
format_latex(h_incent_2bh_eur1, dv_label, trt_label = "EUR 1 vs. control", path = "../figures/h_incent_2bh_eur1.tex", add.cace = FALSE)
format_latex(h_incent_2bh_eur2, dv_label, trt_label = "EUR 2 vs. control", path = "../figures/h_incent_2bh_eur2.tex", add.cace = FALSE)
format_latex(h_incent_2bh_eur5, dv_label, trt_label = "EUR 5 vs. control", path = "../figures/h_incent_2bh_eur5.tex", add.cace = FALSE)
format_latex(h_incent_2bh_eur1_eur_2, dv_label, trt_label = "EUR 2 vs. EUR 1", path = "../figures/h_incent_2bh_eur1_eur_2.tex", add.cace = FALSE)
format_latex(h_incent_2bh_eur1_eur_5, dv_label, trt_label = "EUR 5 vs. EUR 1", path = "../figures/h_incent_2bh_eur1_eur_5.tex", add.cace = FALSE)
format_latex(h_incent_2bh_eur2_eur_5, dv_label, trt_label = "EUR 5 vs. EUR 2", path = "../figures/h_incent_2bh_eur2_eur_5.tex", add.cace = FALSE)




### H3a: Subjects in any of the incentivization groups will be more likely to report using the app than subjects assigned to the control group.
### H3b: The differences between incentivized subjects and the control group referred to in H3a will be larger for subjects offered higher monetary incentives.

# define vars
dv <- "app_rep_installed_at_w3_incent_std"
dv_pre <- "app_rep_installed_at_w2"
dv_label <- "Uptake (reported)"

# run models
h_incent_3a_pooled <- run_models(trt = "treat_incent_any.2", dv = dv, dv_pre = dv_pre, covars = covars2, data = filter(ger_df_wide, !is.na(treat_incent_any.2)), run.cace = FALSE)
h_incent_3b_eur1 <- run_models(trt = "treat_incent_any.2", dv = dv, dv_pre = dv_pre, covars = covars2, data = filter(ger_df_wide, !is.na(treat_incent_any.2), !(treat_incent.2 %in% c("EUR 2", "EUR 5"))), run.cace = FALSE)
h_incent_3b_eur2 <- run_models(trt = "treat_incent_any.2", dv = dv, dv_pre = dv_pre, covars = covars2, data = filter(ger_df_wide, !is.na(treat_incent_any.2), !(treat_incent.2 %in% c("EUR 1", "EUR 5"))), run.cace = FALSE)
h_incent_3b_eur5 <- run_models(trt = "treat_incent_any.2", dv = dv, dv_pre = dv_pre, covars = covars2, data = filter(ger_df_wide, !is.na(treat_incent_any.2), !(treat_incent.2 %in% c("EUR 1", "EUR 2"))), run.cace = FALSE)
h_incent_3b_eur1_eur_2 <- run_models(trt = "treat_incent_eur2.2", dv = dv, dv_pre = dv_pre, covars = covars2, data = filter(ger_df_wide, !is.na(treat_incent_any.2), !(treat_incent.2 %in% c("EUR 5", "Control"))), run.cace = FALSE)
h_incent_3b_eur1_eur_5 <- run_models(trt = "treat_incent_eur5.2", dv = dv, dv_pre = dv_pre, covars = covars2, data = filter(ger_df_wide, !is.na(treat_incent_any.2), !(treat_incent.2 %in% c("EUR 2", "Control"))), run.cace = FALSE)
h_incent_3b_eur2_eur_5 <- run_models(trt = "treat_incent_eur5.2", dv = dv, dv_pre = dv_pre, covars = covars2, data = filter(ger_df_wide, !is.na(treat_incent_any.2), !(treat_incent.2 %in% c("EUR 1", "Control"))), run.cace = FALSE)

# export results tables
format_latex(h_incent_3a_pooled, dv_label, trt_label = "Pooled vs. control", path = "../figures/h_incent_3a_pooled.tex", add.cace = FALSE)
format_latex(h_incent_3b_eur1, dv_label, trt_label = "EUR 1 vs. control", path = "../figures/h_incent_3b_eur1.tex", add.cace = FALSE)
format_latex(h_incent_3b_eur2, dv_label, trt_label = "EUR 2 vs. control", path = "../figures/h_incent_3b_eur2.tex", add.cace = FALSE)
format_latex(h_incent_3b_eur5, dv_label, trt_label = "EUR 5 vs. control", path = "../figures/h_incent_3b_eur5.tex", add.cace = FALSE)
format_latex(h_incent_3b_eur1_eur_2, dv_label, trt_label = "EUR 2 vs. EUR 1", path = "../figures/h_incent_3b_eur1_eur_2.tex", add.cace = FALSE)
format_latex(h_incent_3b_eur1_eur_5, dv_label, trt_label = "EUR 5 vs. EUR 1", path = "../figures/h_incent_3b_eur1_eur_5.tex", add.cace = FALSE)
format_latex(h_incent_3b_eur2_eur_5, dv_label, trt_label = "EUR 5 vs. EUR 2", path = "../figures/h_incent_3b_eur2_eur_5.tex", add.cace = FALSE)




### Hypotheses on downstream effects of Uptake ----------

### H4: Subjects in any of the incentivization groups will show higher levels of knowledge about the app than subjects assigned to the control group.

# define vars
dv <- "app_know_index_std.3"
dv_pre <- "app_know_index.2"
D <- "app_hyb_installed_at_w3" # hybrid Uptake measure, W3
dv_label <- "Knowledge"

# run models
h_incent_4_pooled <- run_models(trt = "treat_incent_any.2", dv = dv, dv_pre = dv_pre, D = D, covars = covars2, data = filter(ger_df_wide, !is.na(treat_incent_any.2)), run.cace = TRUE)
h_incent_4_eur1 <- run_models(trt = "treat_incent_any.2", dv = dv, dv_pre = dv_pre, D = D, covars = covars2, data = filter(ger_df_wide, !is.na(treat_incent_any.2), !(treat_incent.2 %in% c("EUR 2", "EUR 5"))), run.cace = TRUE)
h_incent_4_eur2 <- run_models(trt = "treat_incent_any.2", dv = dv, dv_pre = dv_pre, D = D, covars = covars2, data = filter(ger_df_wide, !is.na(treat_incent_any.2), !(treat_incent.2 %in% c("EUR 1", "EUR 5"))), run.cace = TRUE)
h_incent_4_eur5 <- run_models(trt = "treat_incent_any.2", dv = dv, dv_pre = dv_pre, D = D, covars = covars2, data = filter(ger_df_wide, !is.na(treat_incent_any.2), !(treat_incent.2 %in% c("EUR 1", "EUR 2"))), run.cace = TRUE)
h_incent_4_eur1_eur_2 <- run_models(trt = "treat_incent_eur2.2", dv = dv, dv_pre = dv_pre, D = D, covars = covars2, data = filter(ger_df_wide, !is.na(treat_incent_any.2), !(treat_incent.2 %in% c("EUR 5", "Control"))), run.cace = TRUE)
h_incent_4_eur1_eur_5 <- run_models(trt = "treat_incent_eur5.2", dv = dv, dv_pre = dv_pre, D = D, covars = covars2, data = filter(ger_df_wide, !is.na(treat_incent_any.2), !(treat_incent.2 %in% c("EUR 2", "Control"))), run.cace = TRUE)
h_incent_4_eur2_eur_5 <- run_models(trt = "treat_incent_eur5.2", dv = dv, dv_pre = dv_pre, D = D, covars = covars2, data = filter(ger_df_wide, !is.na(treat_incent_any.2), !(treat_incent.2 %in% c("EUR 1", "Control"))), run.cace = TRUE)

# export results tables
format_latex(h_incent_4_pooled, dv_label, trt_label = "Pooled vs. control", path = "../figures/h_incent_4_pooled.tex", add.cace = TRUE)
format_latex(h_incent_4_eur1, dv_label, trt_label = "EUR 1 vs. control", path = "../figures/h_incent_4_eur1.tex", add.cace = TRUE)
format_latex(h_incent_4_eur2, dv_label, trt_label = "EUR 2 vs. control", path = "../figures/h_incent_4_eur2.tex", add.cace = TRUE)
format_latex(h_incent_4_eur5, dv_label, trt_label = "EUR 5 vs. control", path = "../figures/h_incent_4_eur5.tex", add.cace = TRUE)
format_latex(h_incent_4_eur1_eur_2, dv_label, trt_label = "EUR 2 vs. EUR 1", path = "../figures/h_incent_4_eur1_eur_2.tex", add.cace = TRUE)
format_latex(h_incent_4_eur1_eur_5, dv_label, trt_label = "EUR 5 vs. EUR 1", path = "../figures/h_incent_4_eur1_eur_5.tex", add.cace = TRUE)
format_latex(h_incent_4_eur2_eur_5, dv_label, trt_label = "EUR 5 vs. EUR 2", path = "../figures/h_incent_4_eur2_eur_5.tex", add.cace = TRUE)


### H5: Subjects in any of the incentivization groups will report more positive attitudes towards the app than subjects assigned to the control group.

# define vars
dv <- "att_scale_score_incent_std.3"
dv_pre <- "att_scale_score.2"
D <- "app_hyb_installed_at_w3" # hybrid Uptake measure, W3
dv_label <- "Positive attitudes"

# run models
h_incent_5_pooled <- run_models(trt = "treat_incent_any.2", dv = dv, dv_pre = dv_pre, D = D, covars = covars2, data = filter(ger_df_wide, !is.na(treat_incent_any.2)), run.cace = TRUE)
h_incent_5_eur1 <- run_models(trt = "treat_incent_any.2", dv = dv, dv_pre = dv_pre, D = D, covars = covars2, data = filter(ger_df_wide, !is.na(treat_incent_any.2), !(treat_incent.2 %in% c("EUR 2", "EUR 5"))), run.cace = TRUE)
h_incent_5_eur2 <- run_models(trt = "treat_incent_any.2", dv = dv, dv_pre = dv_pre, D = D, covars = covars2, data = filter(ger_df_wide, !is.na(treat_incent_any.2), !(treat_incent.2 %in% c("EUR 1", "EUR 5"))), run.cace = TRUE)
h_incent_5_eur5 <- run_models(trt = "treat_incent_any.2", dv = dv, dv_pre = dv_pre, D = D, covars = covars2, data = filter(ger_df_wide, !is.na(treat_incent_any.2), !(treat_incent.2 %in% c("EUR 1", "EUR 2"))), run.cace = TRUE)
h_incent_5_eur1_eur_2 <- run_models(trt = "treat_incent_eur2.2", dv = dv, dv_pre = dv_pre, D = D, covars = covars2, data = filter(ger_df_wide, !is.na(treat_incent_any.2), !(treat_incent.2 %in% c("EUR 5", "Control"))), run.cace = TRUE)
h_incent_5_eur1_eur_5 <- run_models(trt = "treat_incent_eur5.2", dv = dv, dv_pre = dv_pre, D = D, covars = covars2, data = filter(ger_df_wide, !is.na(treat_incent_any.2), !(treat_incent.2 %in% c("EUR 2", "Control"))), run.cace = TRUE)
h_incent_5_eur2_eur_5 <- run_models(trt = "treat_incent_eur5.2", dv = dv, dv_pre = dv_pre, D = D, covars = covars2, data = filter(ger_df_wide, !is.na(treat_incent_any.2), !(treat_incent.2 %in% c("EUR 1", "Control"))), run.cace = TRUE)

# export results tables
format_latex(h_incent_5_pooled, dv_label, trt_label = "Pooled vs. control", path = "../figures/h_incent_5_pooled.tex", add.cace = TRUE)
format_latex(h_incent_5_eur1, dv_label, trt_label = "EUR 1 vs. control", path = "../figures/h_incent_5_eur1.tex", add.cace = TRUE)
format_latex(h_incent_5_eur2, dv_label, trt_label = "EUR 2 vs. control", path = "../figures/h_incent_5_eur2.tex", add.cace = TRUE)
format_latex(h_incent_5_eur5, dv_label, trt_label = "EUR 5 vs. control", path = "../figures/h_incent_5_eur5.tex", add.cace = TRUE)
format_latex(h_incent_5_eur1_eur_2, dv_label, trt_label = "EUR 2 vs. EUR 1", path = "../figures/h_incent_5_eur1_eur_2.tex", add.cace = TRUE)
format_latex(h_incent_5_eur1_eur_5, dv_label, trt_label = "EUR 5 vs. EUR 1", path = "../figures/h_incent_5_eur1_eur_5.tex", add.cace = TRUE)
format_latex(h_incent_5_eur2_eur_5, dv_label, trt_label = "EUR 5 vs. EUR 2", path = "../figures/h_incent_5_eur2_eur_5.tex", add.cace = TRUE)


### H6: Subjects in any of the incentivization groups will be more likely to intend to share a message about the app than subjects assigned to the control group.

# define vars
dv <- "app_sharing_clicks_std.2"
dv_pre <- "app_sharing_clicks_std.1"
D <- "app_hyb_installed_at_w3" # hybrid Uptake measure, W3
dv_label <- "Message sharing"

# run models
h_incent_6_pooled <- run_models(trt = "treat_incent_any.2", dv = dv, dv_pre = dv_pre, D = D, covars = covars2, data = filter(ger_df_wide, !is.na(treat_incent_any.2)), run.cace = TRUE)
h_incent_6_eur1 <- run_models(trt = "treat_incent_any.2", dv = dv, dv_pre = dv_pre, D = D, covars = covars2, data = filter(ger_df_wide, !is.na(treat_incent_any.2), !(treat_incent.2 %in% c("EUR 2", "EUR 5"))), run.cace = TRUE)
h_incent_6_eur2 <- run_models(trt = "treat_incent_any.2", dv = dv, dv_pre = dv_pre, D = D, covars = covars2, data = filter(ger_df_wide, !is.na(treat_incent_any.2), !(treat_incent.2 %in% c("EUR 1", "EUR 5"))), run.cace = TRUE)
h_incent_6_eur5 <- run_models(trt = "treat_incent_any.2", dv = dv, dv_pre = dv_pre, D = D, covars = covars2, data = filter(ger_df_wide, !is.na(treat_incent_any.2), !(treat_incent.2 %in% c("EUR 1", "EUR 2"))), run.cace = TRUE)
h_incent_6_eur1_eur_2 <- run_models(trt = "treat_incent_eur2.2", dv = dv, dv_pre = dv_pre, D = D, covars = covars2, data = filter(ger_df_wide, !is.na(treat_incent_any.2), !(treat_incent.2 %in% c("EUR 5", "Control"))), run.cace = TRUE)
h_incent_6_eur1_eur_5 <- run_models(trt = "treat_incent_eur5.2", dv = dv, dv_pre = dv_pre, D = D, covars = covars2, data = filter(ger_df_wide, !is.na(treat_incent_any.2), !(treat_incent.2 %in% c("EUR 2", "Control"))), run.cace = TRUE)
h_incent_6_eur2_eur_5 <- run_models(trt = "treat_incent_eur5.2", dv = dv, dv_pre = dv_pre, D = D, covars = covars2, data = filter(ger_df_wide, !is.na(treat_incent_any.2), !(treat_incent.2 %in% c("EUR 1", "Control"))), run.cace = TRUE)

# export results tables
format_latex(h_incent_6_pooled, dv_label, trt_label = "Pooled vs. control", path = "../figures/h_incent_6_pooled.tex", add.cace = TRUE)
format_latex(h_incent_6_eur1, dv_label, trt_label = "EUR 1 vs. control", path = "../figures/h_incent_6_eur1.tex", add.cace = TRUE)
format_latex(h_incent_6_eur2, dv_label, trt_label = "EUR 2 vs. control", path = "../figures/h_incent_6_eur2.tex", add.cace = TRUE)
format_latex(h_incent_6_eur5, dv_label, trt_label = "EUR 5 vs. control", path = "../figures/h_incent_6_eur5.tex", add.cace = TRUE)
format_latex(h_incent_6_eur1_eur_2, dv_label, trt_label = "EUR 2 vs. EUR 1", path = "../figures/h_incent_6_eur1_eur_2.tex", add.cace = TRUE)
format_latex(h_incent_6_eur1_eur_5, dv_label, trt_label = "EUR 5 vs. EUR 1", path = "../figures/h_incent_6_eur1_eur_5.tex", add.cace = TRUE)
format_latex(h_incent_6_eur2_eur_5, dv_label, trt_label = "EUR 5 vs. EUR 2", path = "../figures/h_incent_6_eur2_eur_5.tex", add.cace = TRUE)





### Exploratory analysis ----------

### H4: Subjects in any of the incentivization groups will show higher levels of applied knowledge about the app than subjects assigned to the control group. (here: alternative knowledge measure - recognition of app icon, app screenshot)

# define vars
dv <- "app_know2_index.3"
dv_pre <- "app_know_icon_cor.2"
D <- "app_hyb_installed_at_w3" # hybrid Uptake measure, W3
dv_label <- "Knowledge (icon, screenshot)"

# run models
h_incent_know2_pooled <- run_models(trt = "treat_incent_any.2", dv = dv, dv_pre = dv_pre, D = D, covars = covars2, data = filter(ger_df_wide, !is.na(treat_incent_any.2)), run.cace = TRUE)
h_incent_know2_eur1 <- run_models(trt = "treat_incent_any.2", dv = dv, dv_pre = dv_pre, D = D, covars = covars2, data = filter(ger_df_wide, !is.na(treat_incent_any.2), !(treat_incent.2 %in% c("EUR 2", "EUR 5"))), run.cace = TRUE)
h_incent_know2_eur2 <- run_models(trt = "treat_incent_any.2", dv = dv, dv_pre = dv_pre, D = D, covars = covars2, data = filter(ger_df_wide, !is.na(treat_incent_any.2), !(treat_incent.2 %in% c("EUR 1", "EUR 5"))), run.cace = TRUE)
h_incent_know2_eur5 <- run_models(trt = "treat_incent_any.2", dv = dv, dv_pre = dv_pre, D = D, covars = covars2, data = filter(ger_df_wide, !is.na(treat_incent_any.2), !(treat_incent.2 %in% c("EUR 1", "EUR 2"))), run.cace = TRUE)
h_incent_know2_eur1_eur_2 <- run_models(trt = "treat_incent_eur2.2", dv = dv, dv_pre = dv_pre, D = D, covars = covars2, data = filter(ger_df_wide, !is.na(treat_incent_any.2), !(treat_incent.2 %in% c("EUR 5", "Control"))), run.cace = TRUE)
h_incent_know2_eur1_eur_5 <- run_models(trt = "treat_incent_eur5.2", dv = dv, dv_pre = dv_pre, D = D, covars = covars2, data = filter(ger_df_wide, !is.na(treat_incent_any.2), !(treat_incent.2 %in% c("EUR 2", "Control"))), run.cace = TRUE)
h_incent_know2_eur2_eur_5 <- run_models(trt = "treat_incent_eur5.2", dv = dv, dv_pre = dv_pre, D = D, covars = covars2, data = filter(ger_df_wide, !is.na(treat_incent_any.2), !(treat_incent.2 %in% c("EUR 1", "Control"))), run.cace = TRUE)

# export results tables
format_latex(h_incent_know2_pooled, dv_label, trt_label = "Pooled vs. control", path = "../figures/h_incent_know2_pooled.tex", add.cace = TRUE)
format_latex(h_incent_know2_eur1, dv_label, trt_label = "EUR 1 vs. control", path = "../figures/h_incent_know2_eur1.tex", add.cace = TRUE)
format_latex(h_incent_know2_eur2, dv_label, trt_label = "EUR 2 vs. control", path = "../figures/h_incent_know2_eur2.tex", add.cace = TRUE)
format_latex(h_incent_know2_eur5, dv_label, trt_label = "EUR 5 vs. control", path = "../figures/h_incent_know2_eur5.tex", add.cace = TRUE)
format_latex(h_incent_know2_eur1_eur_2, dv_label, trt_label = "EUR 2 vs. EUR 1", path = "../figures/h_incent_know2_eur1_eur_2.tex", add.cace = TRUE)
format_latex(h_incent_know2_eur1_eur_5, dv_label, trt_label = "EUR 5 vs. EUR 1", path = "../figures/h_incent_know2_eur1_eur_5.tex", add.cace = TRUE)
format_latex(h_incent_know2_eur2_eur_5, dv_label, trt_label = "EUR 5 vs. EUR 2", path = "../figures/h_incent_know2_eur2_eur_5.tex", add.cace = TRUE)




### Test of heterogeneity hypotheses ----------------------

dv_vec <- c("app_track_installed_at_w3_incent_std", "app_hyb_installed_at_w3_incent_std", "app_rep_installed_at_w3_incent_std", "app_know_index_std.3", "app_know2_index.3", "att_scale_score_incent_std.3", "app_sharing_clicks_std.2")
dv_pre_vec <- c("app_track_installed_at_w2", "app_hyb_installed_at_w2", "app_rep_installed_at_w2", "app_know_index.2", "app_know_icon_cor.2", "att_scale_score.2", "app_sharing_clicks_std.1")
dv_labels <- c("Uptake (tracked)", "Uptake (hybrid)", "Uptake (reported)", "Knowledge", "Knowledge (icon, screenshot)", "Positive attitudes", "Message sharing (tracked)")


### H-Age-Het (not pre-registered): What's the moderating effect of age?

het_age_list <- list()
het_age_effect <- character()
for (i in seq_along(dv_vec)){
  dv <- dv_vec[i]
  dv_pre <- dv_pre_vec[i]
  if(is.na(dv_pre)){ dv_pre <- NULL}
  het_age_list[[i]] <- try(heterogeneous_itt(dv = dv, dv_pre = dv_pre, covars = covars2,
                                             moderator = "age.1", trt = "treat_incent_any.2",
                                             data = filter(ger_df_wide, !is.na(treat_incent_any.2))))
  het_age_effect[i] <- try(het_age_list[[i]][[2]])
}

### H-NPI-Het (not pre-registered): What's the moderating effect of general NPI compliance?

het_npi_list <- list()
het_npi_effect <- character()
for (i in seq_along(dv_vec)){
  dv <- dv_vec[i]
  dv_pre <- dv_pre_vec[i]
  if(is.na(dv_pre)){ dv_pre <- NULL}
  het_npi_list[[i]] <- try(heterogeneous_itt(dv = dv, dv_pre = dv_pre, covars = covars2,
                                             moderator = "aha_compliance_cat2.1", trt = "treat_incent_any.2",
                                             data = filter(ger_df_wide, !is.na(treat_incent_any.2))))
  het_npi_effect[i] <- try(het_npi_list[[i]][[2]])
}

### H-GovTrust-Het (not pre-registered): What's the moderating effect of trust in government?

het_govtrust_list <- list()
het_govtrust_effect <- character()
for (i in seq_along(dv_vec)){
  dv <- dv_vec[i]
  dv_pre <- dv_pre_vec[i]
  if(is.na(dv_pre)){ dv_pre <- NULL}
  het_govtrust_list[[i]] <- try(heterogeneous_itt(dv = dv, dv_pre = dv_pre, covars = covars2,
                                                  moderator = "trust_gov_cat2.1", trt = "treat_incent_any.2",
                                                  data = filter(ger_df_wide, !is.na(treat_incent_any.2))))
  het_govtrust_effect[i] <- try(het_govtrust_list[[i]][[2]])
}


### H-ThreatPerception-Het (not pre-registered): What's the moderating effect of COVID threat perception?

het_threatfamf_list <- list()
het_threatfamf_effect <- character()
for (i in seq_along(dv_vec)){
  dv <- dv_vec[i]
  dv_pre <- dv_pre_vec[i]
  if(is.na(dv_pre)){ dv_pre <- NULL}
  het_threatfamf_list[[i]] <- try(heterogeneous_itt(dv = dv, dv_pre = dv_pre, covars = covars2,
                                                    moderator = "covidconcerned_famf_cat2.1", trt = "treat_incent_any.2",
                                                    data = filter(ger_df_wide, !is.na(treat_incent_any.2))))
  het_threatfamf_effect[i] <- try(het_threatfamf_list[[i]][[2]])
}


### What's the moderating effect of message treatment status?

het_messagetreat_list <- list()
het_messagetreat_effect <- character()
for (i in seq_along(dv_vec)){
  dv <- dv_vec[i]
  dv_pre <- dv_pre_vec[i]
  if(is.na(dv_pre)){ dv_pre <- NULL}
  het_messagetreat_list[[i]] <- try(heterogeneous_itt(dv = dv, dv_pre = dv_pre, covars = covars2,
                                                      moderator = "treat_any.1", trt = "treat_incent_any.2",
                                                      data = filter(ger_df_wide, !is.na(treat_incent_any.2))))
  het_messagetreat_effect[i] <- try(het_messagetreat_list[[i]][[2]])
}



### What's the moderating effect of self-interest?

het_selfint_list <- list()
het_selfint_effect <- character()
for (i in seq_along(dv_vec)){
  dv <- dv_vec[i]
  dv_pre <- dv_pre_vec[i]
  if(is.na(dv_pre)){ dv_pre <- NULL}
  het_selfint_list[[i]] <- try(heterogeneous_itt(dv = dv, dv_pre = dv_pre, covars = covars2,
                                                      moderator = "selfint_score.1", trt = "treat_incent_any.2",
                                                      data = filter(ger_df_wide, !is.na(treat_incent_any.2))))
  het_selfint_effect[i] <- try(het_selfint_list[[i]][[2]])
}




# print to latex
het_effects_df <- 
  data.frame(Outcome = dv_labels,
             Age = het_age_effect,
             NPI = het_npi_effect,
             GovTrust = het_govtrust_effect,
             MessageTreat = het_messagetreat_effect,
             stringsAsFactors = FALSE
  )
print(xtable(het_effects_df), include.rownames = FALSE)



