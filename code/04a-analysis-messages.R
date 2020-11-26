### -----------------------------
### analysis of message intervention
### -----------------------------

# load data --------------------------

ger_df_wide <- readRDS("../data/clean/ger_df_wide_an.RDS")



### Test of main hypotheses ----------------------

### H-TC-1a: Subjects assigned to any of the treatments will be more likely to be observed to install the app than subjects assigned to the control group.
### only surveytracking sample

# define vars
dv <- "app_track_installed_before_w2_std"
dv_pre <- "app_track_installed_before_w1"
D <- "treat_comp.1"
dv_label <- "Uptake (tracked)"
dat_surveytracking <- filter(ger_df_wide, sample_type.1 == "surveytracking")

# run models
h_tc_1a_pooled <- run_models(trt = "treat_any.1", dv, dv_pre, covars, D, dat_surveytracking)
h_tc_1a_prosoc <- run_models(trt = "treat_prosoc.1", dv, dv_pre, covars, D, filter(dat_surveytracking, treat.1 != "selfinterest"))
h_tc_1a_selfint <- run_models(trt = "treat_selfint.1", dv, dv_pre, covars, D, filter(dat_surveytracking, treat.1 != "prosocial"))
h_tt_1a <- run_models(trt = "treat_prosoc.1", dv, dv_pre, covars, D, filter(dat_surveytracking, treat.1 != "control"))

# run models, only with people in treatment groups who pass manipulation check
h_tc_1a_pooled_check <- run_models(trt = "treat_any.1", dv, dv_pre, covars, D, filter(dat_surveytracking, manipcheck_pass.1 == TRUE))
h_tc_1a_prosoc_check <- run_models(trt = "treat_prosoc.1", dv, dv_pre, covars, D, filter(dat_surveytracking, treat.1 != "selfinterest", manipcheck_pass.1 == TRUE))
h_tc_1a_selfint_check <- run_models(trt = "treat_selfint.1", dv, dv_pre, covars, D, filter(dat_surveytracking, treat.1 != "prosocial", manipcheck_pass.1 == TRUE))
h_tt_1a_check <- run_models(trt = "treat_prosoc.1", dv, dv_pre, covars, D, filter(dat_surveytracking, treat.1 != "control", manipcheck_pass.1 == TRUE))

# export results tables
format_latex(h_tc_1a_pooled, dv_label, trt_label = "Pooled vs. control", path = "../figures/h_tc_1a_pooled.tex")
format_latex(h_tc_1a_prosoc, dv_label, trt_label = "Prosocial vs. control", path = "../figures/h_tc_1a_prosoc.tex")
format_latex(h_tc_1a_selfint, dv_label, trt_label = "Self-interest vs. control", path = "../figures/h_tc_1a_selfint.tex")
format_latex(h_tt_1a, dv_label, trt_label = "Prosocial vs. self-interest", path = "../figures/h_tt_1a.tex")
format_latex(h_tc_1a_pooled_check, dv_label, trt_label = "Pooled vs. control", path = "../figures/h_tc_1a_pooled_check.tex")
format_latex(h_tc_1a_prosoc_check, dv_label, trt_label = "Prosocial vs. control", path = "../figures/h_tc_1a_prosoc_check.tex")
format_latex(h_tc_1a_selfint_check, dv_label, trt_label = "Self-interest vs. control", path = "../figures/h_tc_1a_selfint_check.tex")
format_latex(h_tt_1a_check, dv_label, trt_label = "Prosocial vs. self-interest", path = "../figures/h_tt_1a_check.tex")



### H-TC-1b: Subjects assigned to any of the treatments will be more likely to be observed to install the app than subjects assigned to the control group.
### surveytracking + trackingonly sample; reduced set of controls

# define vars
dv <- "app_track_installed_before_w2_std"
dv_pre <- "app_track_installed_before_w1"
D <- "treat_comp.1"
dv_label <- "Uptake (tracked)"
dat_surveytrackingplus <- filter(ger_df_wide, sample_type.1 == "surveytracking" | sample_type.1 == "trackingonly")

# run models
h_tc_1b_pooled <- run_models(trt = "treat_any.1", dv, dv_pre, covars_red, D, dat_surveytrackingplus)
h_tc_1b_prosoc <- run_models(trt = "treat_prosoc.1", dv, dv_pre, covars_red, D, filter(dat_surveytrackingplus, treat.1 != "selfinterest"))
h_tc_1b_selfint <- run_models(trt = "treat_selfint.1", dv, dv_pre, covars_red, D, filter(dat_surveytrackingplus, treat.1 != "prosocial"))
h_tt_1b <- run_models(trt = "treat_prosoc.1", dv, dv_pre, covars_red, D, filter(dat_surveytrackingplus, treat.1 != "control"))

# run models, only with people in treatment groups who pass manipulation check
h_tc_1b_pooled_check <- run_models(trt = "treat_any.1", dv, dv_pre, covars_red, D, filter(dat_surveytrackingplus, manipcheck_pass.1 == TRUE))
h_tc_1b_prosoc_check <- run_models(trt = "treat_prosoc.1", dv, dv_pre, covars_red, D, filter(dat_surveytrackingplus, treat.1 != "selfinterest", manipcheck_pass.1 == TRUE))
h_tc_1b_selfint_check <- run_models(trt = "treat_selfint.1", dv, dv_pre, covars_red, D, filter(dat_surveytrackingplus, treat.1 != "prosocial", manipcheck_pass.1 == TRUE))
h_tt_1b_check <- run_models(trt = "treat_prosoc.1", dv, dv_pre, covars_red, D, filter(dat_surveytrackingplus, treat.1 != "control", manipcheck_pass.1 == TRUE))


# export results tables
format_latex(h_tc_1b_pooled, dv_label, trt_label = "Pooled vs. control", path = "../figures/h_tc_1b_pooled.tex")
format_latex(h_tc_1b_prosoc, dv_label, trt_label = "Prosocial vs. control", path = "../figures/h_tc_1b_prosoc.tex")
format_latex(h_tc_1b_selfint, dv_label, trt_label = "Self-interest vs. control", path = "../figures/h_tc_1b_selfint.tex")
format_latex(h_tt_1b, dv_label, trt_label = "Prosocial vs. self-interest", path = "../figures/h_tt_1b.tex")
format_latex(h_tc_1b_pooled_check, dv_label, trt_label = "Pooled vs. control", path = "../figures/h_tc_1b_pooled_check.tex")
format_latex(h_tc_1b_prosoc_check, dv_label, trt_label = "Prosocial vs. control", path = "../figures/h_tc_1b_prosoc_check.tex")
format_latex(h_tc_1b_selfint_check, dv_label, trt_label = "Self-interest vs. control", path = "../figures/h_tc_1b_selfint_check.tex")
format_latex(h_tt_1b_check, dv_label, trt_label = "Prosocial vs. self-interest", path = "../figures/h_tt_1b_check.tex")



### H-TC-1c: Subjects assigned to any of the treatments will be more likely to be observed to install the app than subjects assigned to the control group.
### surveytracking + surveyonly + trackingonly sample; hybrid outcome measure; reduced set of controls

# define vars
dv <- "app_hyb_installed_before_w2_std"
dv_pre <- "app_hyb_installed_before_w1"
D <- "treat_comp.1"
dv_label <- "Uptake (hybrid)"

# run models
h_tc_1c_pooled <- run_models(trt = "treat_any.1", dv, dv_pre, covars_red, D, ger_df_wide)
h_tc_1c_prosoc <- run_models(trt = "treat_prosoc.1", dv, dv_pre, covars_red, D, filter(ger_df_wide, treat.1 != "selfinterest"))
h_tc_1c_selfint <- run_models(trt = "treat_selfint.1", dv, dv_pre, covars_red, D, filter(ger_df_wide, treat.1 != "prosocial"))
h_tt_1c <- run_models(trt = "treat_prosoc.1", dv, dv_pre, covars_red, D, filter(ger_df_wide, treat.1 != "control"))

# run models, only with people in treatment groups who pass manipulation check
h_tc_1c_pooled_check <- run_models(trt = "treat_any.1", dv, dv_pre, covars_red, D, filter(ger_df_wide, manipcheck_pass.1 == TRUE))
h_tc_1c_prosoc_check <- run_models(trt = "treat_prosoc.1", dv, dv_pre, covars_red, D, filter(ger_df_wide, treat.1 != "selfinterest", manipcheck_pass.1 == TRUE))
h_tc_1c_selfint_check <- run_models(trt = "treat_selfint.1", dv, dv_pre, covars_red, D, filter(ger_df_wide, treat.1 != "prosocial", manipcheck_pass.1 == TRUE))
h_tt_1c_check <- run_models(trt = "treat_prosoc.1", dv, dv_pre, covars_red, D, filter(ger_df_wide, treat.1 != "control", manipcheck_pass.1 == TRUE))

# export results tables
format_latex(h_tc_1c_pooled, dv_label, trt_label = "Pooled vs. control", path = "../figures/h_tc_1c_pooled.tex")
format_latex(h_tc_1c_prosoc, dv_label, trt_label = "Prosocial vs. control", path = "../figures/h_tc_1c_prosoc.tex")
format_latex(h_tc_1c_selfint, dv_label, trt_label = "Self-interest vs. control", path = "../figures/h_tc_1c_selfint.tex")
format_latex(h_tt_1c, dv_label, trt_label = "Prosocial vs. self-interest", path = "../figures/h_tt_1c.tex")
format_latex(h_tc_1c_pooled_check, dv_label, trt_label = "Pooled vs. control", path = "../figures/h_tc_1c_pooled_check.tex")
format_latex(h_tc_1c_prosoc_check, dv_label, trt_label = "Prosocial vs. control", path = "../figures/h_tc_1c_prosoc_check.tex")
format_latex(h_tc_1c_selfint_check, dv_label, trt_label = "Self-interest vs. control", path = "../figures/h_tc_1c_selfint_check.tex")
format_latex(h_tt_1c_check, dv_label, trt_label = "Prosocial vs. self-interest", path = "../figures/h_tt_1c_check.tex")



### H-TC-2: Subjects assigned to any of the treatments will be more likely to report using the app than subjects assigned to the control group.

# define vars
dv <- "app_rep_installed_before_w2_std"
dv_pre <- "app_rep_installed_before_w1"
D <- "treat_comp.1"
dv_label <- "Uptake (reported)"

# run models
h_tc_2_pooled <- run_models(trt = "treat_any.1", dv, dv_pre, covars, D, ger_df_wide)
h_tc_2_prosoc <- run_models(trt = "treat_prosoc.1", dv, dv_pre, covars, D, filter(ger_df_wide, treat.1 != "selfinterest"))
h_tc_2_selfint <- run_models(trt = "treat_selfint.1", dv, dv_pre, covars, D, filter(ger_df_wide, treat.1 != "prosocial"))
h_tt_2 <- run_models(trt = "treat_prosoc.1", dv, dv_pre, covars, D, filter(ger_df_wide, treat.1 != "control"))

# run models, only with people in treatment groups who pass manipulation check
h_tc_2_pooled_check <- run_models(trt = "treat_any.1", dv, dv_pre, covars, D, filter(ger_df_wide, manipcheck_pass.1 == TRUE))
h_tc_2_prosoc_check <- run_models(trt = "treat_prosoc.1", dv, dv_pre, covars, D, filter(ger_df_wide, treat.1 != "selfinterest", manipcheck_pass.1 == TRUE))
h_tc_2_selfint_check <- run_models(trt = "treat_selfint.1", dv, dv_pre, covars, D, filter(ger_df_wide, treat.1 != "prosocial", manipcheck_pass.1 == TRUE))
h_tt_2_check <- run_models(trt = "treat_prosoc.1", dv, dv_pre, covars, D, filter(ger_df_wide, treat.1 != "control", manipcheck_pass.1 == TRUE))

# export results tables
format_latex(h_tc_2_pooled, dv_label, trt_label = "Pooled vs. control", path = "../figures/h_tc_2_pooled.tex")
format_latex(h_tc_2_prosoc, dv_label, trt_label = "Prosocial vs. control", path = "../figures/h_tc_2_prosoc.tex")
format_latex(h_tc_2_selfint, dv_label, trt_label = "Self-interest vs. control", path = "../figures/h_tc_2_selfint.tex")
format_latex(h_tt_2, dv_label, trt_label = "Prosocial vs. self-interest", path = "../figures/h_tt_2.tex")
format_latex(h_tc_2_pooled_check, dv_label, trt_label = "Pooled vs. control", path = "../figures/h_tc_2_pooled_check.tex")
format_latex(h_tc_2_prosoc_check, dv_label, trt_label = "Prosocial vs. control", path = "../figures/h_tc_2_prosoc_check.tex")
format_latex(h_tc_2_selfint_check, dv_label, trt_label = "Self-interest vs. control", path = "../figures/h_tc_2_selfint_check.tex")
format_latex(h_tt_2_check, dv_label, trt_label = "Prosocial vs. self-interest", path = "../figures/h_tt_2_check.tex")



### H-TC-3: Subjects assigned to any of the treatments will be more likely to report having their smartphone’s bluetooth function activated than subjects assigned to the control group.

# define vars
dv <- "app_bluetooth_act_std.2"
dv_pre <- "app_bluetooth_act.1"
D <- "treat_comp.1"
dv_label <- "Bluetooth active (reported)"

# run models
h_tc_3_pooled <- run_models(trt = "treat_any.1", dv, dv_pre, covars, D, ger_df_wide)
h_tc_3_prosoc <- run_models(trt = "treat_prosoc.1", dv, dv_pre, covars, D, filter(ger_df_wide, treat.1 != "selfinterest"))
h_tc_3_selfint <- run_models(trt = "treat_selfint.1", dv, dv_pre, covars, D, filter(ger_df_wide, treat.1 != "prosocial"))
h_tt_3 <- run_models(trt = "treat_prosoc.1", dv, dv_pre, covars, D, filter(ger_df_wide, treat.1 != "control"))

# run models, only with people in treatment groups who pass manipulation check
h_tc_3_pooled_check <- run_models(trt = "treat_any.1", dv, dv_pre, covars, D, filter(ger_df_wide, manipcheck_pass.1 == TRUE))
h_tc_3_prosoc_check <- run_models(trt = "treat_prosoc.1", dv, dv_pre, covars, D, filter(ger_df_wide, treat.1 != "selfinterest", manipcheck_pass.1 == TRUE))
h_tc_3_selfint_check <- run_models(trt = "treat_selfint.1", dv, dv_pre, covars, D, filter(ger_df_wide, treat.1 != "prosocial", manipcheck_pass.1 == TRUE))
h_tt_3_check <- run_models(trt = "treat_prosoc.1", dv, dv_pre, covars, D, filter(ger_df_wide, treat.1 != "control", manipcheck_pass.1 == TRUE))

# export results tables
format_latex(h_tc_3_pooled, dv_label, trt_label = "Pooled vs. control", path = "../figures/h_tc_3_pooled.tex")
format_latex(h_tc_3_prosoc, dv_label, trt_label = "Prosocial vs. control", path = "../figures/h_tc_3_prosoc.tex")
format_latex(h_tc_3_selfint, dv_label, trt_label = "Self-interest vs. control", path = "../figures/h_tc_3_selfint.tex")
format_latex(h_tt_3, dv_label, trt_label = "Prosocial vs. self-interest", path = "../figures/h_tt_3.tex")
format_latex(h_tc_3_pooled_check, dv_label, trt_label = "Pooled vs. control", path = "../figures/h_tc_3_pooled_check.tex")
format_latex(h_tc_3_prosoc_check, dv_label, trt_label = "Prosocial vs. control", path = "../figures/h_tc_3_prosoc_check.tex")
format_latex(h_tc_3_selfint_check, dv_label, trt_label = "Self-interest vs. control", path = "../figures/h_tc_3_selfint_check.tex")
format_latex(h_tt_3_check, dv_label, trt_label = "Prosocial vs. self-interest", path = "../figures/h_tt_3_check.tex")



### H-TC-4: Subjects assigned to any of the treatments will report a higher likelihood of installing the app in the future than subjects assigned to the control group. 
## POST-TREATMENT MEASURE FUCKED UP: NOBODY GOT THAT ITEM POST-TREATMENT
# 
# dv <- "app_likely_install_std.2"
# dv_pre <- "app_likely_install.1"
# D <- "treat_comp.1"
# dv_label <- "Likelihood to install app"
# 
# # run models
# h_tc_4_pooled <- run_models(trt = "treat_any.1", dv, dv_pre, covars, D, ger_df_wide)
# h_tc_4_prosoc <- run_models(trt = "treat_prosoc.1", dv, dv_pre, covars, D, filter(ger_df_wide, treat.1 != "selfinterest"))
# h_tc_4_selfint <- run_models(trt = "treat_selfint.1", dv, dv_pre, covars, D, filter(ger_df_wide, treat.1 != "prosocial"))
# h_tt_4 <- run_models(trt = "treat_prosoc.1", dv, dv_pre, covars, D, filter(ger_df_wide, treat.1 != "control"))
# 
# # run models, only with people in treatment groups who pass manipulation check
# h_tc_4_pooled_check <- run_models(trt = "treat_any.1", dv, dv_pre, covars, D, filter(ger_df_wide, manipcheck_pass.1 == TRUE))
# h_tc_4_prosoc_check <- run_models(trt = "treat_prosoc.1", dv, dv_pre, covars, D, filter(ger_df_wide, treat.1 != "selfinterest", manipcheck_pass.1 == TRUE))
# h_tc_4_selfint_check <- run_models(trt = "treat_selfint.1", dv, dv_pre, covars, D, filter(ger_df_wide, treat.1 != "prosocial", manipcheck_pass.1 == TRUE))
# h_tt_4_check <- run_models(trt = "treat_prosoc.1", dv, dv_pre, covars, D, filter(ger_df_wide, treat.1 != "control", manipcheck_pass.1 == TRUE))



### H-TC-5: Subjects assigned to any of the treatments will show higher levels of knowledge about the app than subjects assigned to the control group.

# define vars
dv <- "app_know_index_std.2" # app_know_index_std.1 is post-treatment and only used as manipulation check
dv_pre <- NULL
D <- "treat_comp.1"
dv_label <- "Knowledge"

# run models
h_tc_5_pooled <- run_models(trt = "treat_any.1", dv, dv_pre, covars, D, ger_df_wide)
h_tc_5_prosoc <- run_models(trt = "treat_prosoc.1", dv, dv_pre, covars, D, filter(ger_df_wide, treat.1 != "selfinterest"))
h_tc_5_selfint <- run_models(trt = "treat_selfint.1", dv, dv_pre, covars, D, filter(ger_df_wide, treat.1 != "prosocial"))
h_tt_5 <- run_models(trt = "treat_prosoc.1", dv, dv_pre, covars, D, filter(ger_df_wide, treat.1 != "control"))

# run models, only with people in treatment groups who pass manipulation check
h_tc_5_pooled_check <- run_models(trt = "treat_any.1", dv, dv_pre, covars, D, filter(ger_df_wide, manipcheck_pass.1 == TRUE))
h_tc_5_prosoc_check <- run_models(trt = "treat_prosoc.1", dv, dv_pre, covars, D, filter(ger_df_wide, treat.1 != "selfinterest", manipcheck_pass.1 == TRUE))
h_tc_5_selfint_check <- run_models(trt = "treat_selfint.1", dv, dv_pre, covars, D, filter(ger_df_wide, treat.1 != "prosocial", manipcheck_pass.1 == TRUE))
h_tt_5_check <- run_models(trt = "treat_prosoc.1", dv, dv_pre, covars, D, filter(ger_df_wide, treat.1 != "control", manipcheck_pass.1 == TRUE))

# export results tables
format_latex(h_tc_5_pooled, dv_label, trt_label = "Pooled vs. control", path = "../figures/h_tc_5_pooled.tex")
format_latex(h_tc_5_prosoc, dv_label, trt_label = "Prosocial vs. control", path = "../figures/h_tc_5_prosoc.tex")
format_latex(h_tc_5_selfint, dv_label, trt_label = "Self-interest vs. control", path = "../figures/h_tc_5_selfint.tex")
format_latex(h_tt_5, dv_label, trt_label = "Prosocial vs. self-interest", path = "../figures/h_tt_5.tex")
format_latex(h_tc_5_pooled_check, dv_label, trt_label = "Pooled vs. control", path = "../figures/h_tc_5_pooled_check.tex")
format_latex(h_tc_5_prosoc_check, dv_label, trt_label = "Prosocial vs. control", path = "../figures/h_tc_5_prosoc_check.tex")
format_latex(h_tc_5_selfint_check, dv_label, trt_label = "Self-interest vs. control", path = "../figures/h_tc_5_selfint_check.tex")
format_latex(h_tt_5_check, dv_label, trt_label = "Prosocial vs. self-interest", path = "../figures/h_tt_5_check.tex")



### H-TC-6: Subjects assigned to any of the treatments will report more positive attitudes towards the app than subjects assigned to the control group.

# define vars
dv <- "att_scale_score_std.2"
dv_pre <- "att_scale_score.1"
D <- "treat_comp.1"
dv_label <- "Positive attitudes"

# run models
h_tc_6_pooled <- run_models(trt = "treat_any.1", dv, dv_pre, covars, D, ger_df_wide)
h_tc_6_prosoc <- run_models(trt = "treat_prosoc.1", dv, dv_pre, covars, D, filter(ger_df_wide, treat.1 != "selfinterest"))
h_tc_6_selfint <- run_models(trt = "treat_selfint.1", dv, dv_pre, covars, D, filter(ger_df_wide, treat.1 != "prosocial"))
h_tt_6 <- run_models(trt = "treat_prosoc.1", dv, dv_pre, covars, D, filter(ger_df_wide, treat.1 != "control"))

# run models, only with people in treatment groups who pass manipulation check
h_tc_6_pooled_check <- run_models(trt = "treat_any.1", dv, dv_pre, covars, D, filter(ger_df_wide, manipcheck_pass.1 == TRUE))
h_tc_6_prosoc_check <- run_models(trt = "treat_prosoc.1", dv, dv_pre, covars, D, filter(ger_df_wide, treat.1 != "selfinterest", manipcheck_pass.1 == TRUE))
h_tc_6_selfint_check <- run_models(trt = "treat_selfint.1", dv, dv_pre, covars, D, filter(ger_df_wide, treat.1 != "prosocial", manipcheck_pass.1 == TRUE))
h_tt_6_check <- run_models(trt = "treat_prosoc.1", dv, dv_pre, covars, D, filter(ger_df_wide, treat.1 != "control", manipcheck_pass.1 == TRUE))

# export results tables
format_latex(h_tc_6_pooled, dv_label, trt_label = "Pooled vs. control", path = "../figures/h_tc_6_pooled.tex")
format_latex(h_tc_6_prosoc, dv_label, trt_label = "Prosocial vs. control", path = "../figures/h_tc_6_prosoc.tex")
format_latex(h_tc_6_selfint, dv_label, trt_label = "Self-interest vs. control", path = "../figures/h_tc_6_selfint.tex")
format_latex(h_tt_6, dv_label, trt_label = "Prosocial vs. self-interest", path = "../figures/h_tt_6.tex")
format_latex(h_tc_6_pooled_check, dv_label, trt_label = "Pooled vs. control", path = "../figures/h_tc_6_pooled_check.tex")
format_latex(h_tc_6_prosoc_check, dv_label, trt_label = "Prosocial vs. control", path = "../figures/h_tc_6_prosoc_check.tex")
format_latex(h_tc_6_selfint_check, dv_label, trt_label = "Self-interest vs. control", path = "../figures/h_tc_6_selfint_check.tex")
format_latex(h_tt_6_check, dv_label, trt_label = "Prosocial vs. self-interest", path = "../figures/h_tt_6_check.tex")



### H-TC-7: Subjects assigned to any of the treatments will be more likely to intend to share a message about the app than subjects assigned to the control group.

# define vars
dv <- "app_sharing_clicks_std.1"
dv_pre <- NULL
D <- "treat_comp.1"
dv_label <- "Message sharing"

# run models
h_tc_7_pooled <- run_models(trt = "treat_any.1", dv, dv_pre, covars, D, ger_df_wide)
h_tc_7_prosoc <- run_models(trt = "treat_prosoc.1", dv, dv_pre, covars, D, filter(ger_df_wide, treat.1 != "selfinterest"))
h_tc_7_selfint <- run_models(trt = "treat_selfint.1", dv, dv_pre, covars, D, filter(ger_df_wide, treat.1 != "prosocial"))
h_tt_7 <- run_models(trt = "treat_prosoc.1", dv, dv_pre, covars, D, filter(ger_df_wide, treat.1 != "control"))

# run models, only with people in treatment groups who pass manipulation check
h_tc_7_pooled_check <- run_models(trt = "treat_any.1", dv, dv_pre, covars, D, filter(ger_df_wide, manipcheck_pass.1 == TRUE))
h_tc_7_prosoc_check <- run_models(trt = "treat_prosoc.1", dv, dv_pre, covars, D, filter(ger_df_wide, treat.1 != "selfinterest", manipcheck_pass.1 == TRUE))
h_tc_7_selfint_check <- run_models(trt = "treat_selfint.1", dv, dv_pre, covars, D, filter(ger_df_wide, treat.1 != "prosocial", manipcheck_pass.1 == TRUE))
h_tt_7_check <- run_models(trt = "treat_prosoc.1", dv, dv_pre, covars, D, filter(ger_df_wide, treat.1 != "control", manipcheck_pass.1 == TRUE))

# export results tables
format_latex(h_tc_7_pooled, dv_label, trt_label = "Pooled vs. control", path = "../figures/h_tc_7_pooled.tex")
format_latex(h_tc_7_prosoc, dv_label, trt_label = "Prosocial vs. control", path = "../figures/h_tc_7_prosoc.tex")
format_latex(h_tc_7_selfint, dv_label, trt_label = "Self-interest vs. control", path = "../figures/h_tc_7_selfint.tex")
format_latex(h_tt_7, dv_label, trt_label = "Prosocial vs. self-interest", path = "../figures/h_tt_7.tex")
format_latex(h_tc_7_pooled_check, dv_label, trt_label = "Pooled vs. control", path = "../figures/h_tc_7_pooled_check.tex")
format_latex(h_tc_7_prosoc_check, dv_label, trt_label = "Prosocial vs. control", path = "../figures/h_tc_7_prosoc_check.tex")
format_latex(h_tc_7_selfint_check, dv_label, trt_label = "Self-interest vs. control", path = "../figures/h_tc_7_selfint_check.tex")
format_latex(h_tt_7_check, dv_label, trt_label = "Prosocial vs. self-interest", path = "../figures/h_tt_7_check.tex")



### H-TC-8: Subjects assigned to any of the treatments will be more likely to intend to look up additional information about the app than subjects assigned to the control group.

# define vars
dv <- "app_info_clicks_std.1"
dv_pre <- NULL
D <- "treat_comp.1"
dv_label <- "Information lookup"

# run models
h_tc_8_pooled <- run_models(trt = "treat_any.1", dv, dv_pre, covars, D, ger_df_wide)
h_tc_8_prosoc <- run_models(trt = "treat_prosoc.1", dv, dv_pre, covars, D, filter(ger_df_wide, treat.1 != "selfinterest"))
h_tc_8_selfint <- run_models(trt = "treat_selfint.1", dv, dv_pre, covars, D, filter(ger_df_wide, treat.1 != "prosocial"))
h_tt_8 <- run_models(trt = "treat_prosoc.1", dv, dv_pre, covars, D, filter(ger_df_wide, treat.1 != "control"))

# run models, only with people in treatment groups who pass manipulation check
h_tc_8_pooled_check <- run_models(trt = "treat_any.1", dv, dv_pre, covars, D, filter(ger_df_wide, manipcheck_pass.1 == TRUE))
h_tc_8_prosoc_check <- run_models(trt = "treat_prosoc.1", dv, dv_pre, covars, D, filter(ger_df_wide, treat.1 != "selfinterest", manipcheck_pass.1 == TRUE))
h_tc_8_selfint_check <- run_models(trt = "treat_selfint.1", dv, dv_pre, covars, D, filter(ger_df_wide, treat.1 != "prosocial", manipcheck_pass.1 == TRUE))
h_tt_8_check <- run_models(trt = "treat_prosoc.1", dv, dv_pre, covars, D, filter(ger_df_wide, treat.1 != "control", manipcheck_pass.1 == TRUE))

# export results tables
format_latex(h_tc_8_pooled, dv_label, trt_label = "Pooled vs. control", path = "../figures/h_tc_8_pooled.tex")
format_latex(h_tc_8_prosoc, dv_label, trt_label = "Prosocial vs. control", path = "../figures/h_tc_8_prosoc.tex")
format_latex(h_tc_8_selfint, dv_label, trt_label = "Self-interest vs. control", path = "../figures/h_tc_8_selfint.tex")
format_latex(h_tt_8, dv_label, trt_label = "Prosocial vs. self-interest", path = "../figures/h_tt_8.tex")
format_latex(h_tc_8_pooled_check, dv_label, trt_label = "Pooled vs. control", path = "../figures/h_tc_8_pooled_check.tex")
format_latex(h_tc_8_prosoc_check, dv_label, trt_label = "Prosocial vs. control", path = "../figures/h_tc_8_prosoc_check.tex")
format_latex(h_tc_8_selfint_check, dv_label, trt_label = "Self-interest vs. control", path = "../figures/h_tc_8_selfint_check.tex")
format_latex(h_tt_8_check, dv_label, trt_label = "Prosocial vs. self-interest", path = "../figures/h_tt_8_check.tex")



### Test of additional outcomes (not pre-registered) ----------------------

## likelihood of getting tested/quarantined after warning

# define vars
dv <- "app_test_warning_std.2"
dv_pre <- "app_test_warning.1"
D <- "treat_comp.1"
dv_label <- "Tested after warning"

# run models
h_tc_lik_get_test_pooled <- run_models(trt = "treat_any.1", dv, dv_pre, covars, D, ger_df_wide)
h_tc_lik_get_test_prosoc <- run_models(trt = "treat_prosoc.1", dv, dv_pre, covars, D, filter(ger_df_wide, treat.1 != "selfinterest"))
h_tc_lik_get_test_selfint <- run_models(trt = "treat_selfint.1", dv, dv_pre, covars, D, filter(ger_df_wide, treat.1 != "prosocial"))
h_tt_get_test <- run_models(trt = "treat_prosoc.1", dv, dv_pre, covars, D, filter(ger_df_wide, treat.1 != "control"))

# run models, only with people in treatment groups who pass manipulation check
h_tc_lik_get_test_pooled_check <- run_models(trt = "treat_any.1", dv, dv_pre, covars, D, filter(ger_df_wide, manipcheck_pass.1 == TRUE))
h_tc_lik_get_test_prosoc_check <- run_models(trt = "treat_prosoc.1", dv, dv_pre, covars, D, filter(ger_df_wide, treat.1 != "selfinterest", manipcheck_pass.1 == TRUE))
h_tc_lik_get_test_selfint_check <- run_models(trt = "treat_selfint.1", dv, dv_pre, covars, D, filter(ger_df_wide, treat.1 != "prosocial", manipcheck_pass.1 == TRUE))
h_tt_lik_get_test_check <- run_models(trt = "treat_prosoc.1", dv, dv_pre, covars, D, filter(ger_df_wide, treat.1 != "control", manipcheck_pass.1 == TRUE))

# export results tables
format_latex(h_tc_lik_get_test_pooled, dv_label, trt_label = "Pooled vs. control", path = "../figures/h_tc_lik_get_test_pooled.tex")
format_latex(h_tc_lik_get_test_prosoc, dv_label, trt_label = "Prosocial vs. control", path = "../figures/h_tc_lik_get_test_prosoc.tex")
format_latex(h_tc_lik_get_test_selfint, dv_label, trt_label = "Self-interest vs. control", path = "../figures/h_tc_lik_get_test_selfint.tex")
format_latex(h_tt_get_test, dv_label, trt_label = "Prosocial vs. self-interest", path = "../figures/h_tt_lik_get_test.tex")
format_latex(h_tc_lik_get_test_pooled_check, dv_label, trt_label = "Pooled vs. control", path = "../figures/h_tc_lik_get_test_pooled_check.tex")
format_latex(h_tc_lik_get_test_prosoc_check, dv_label, trt_label = "Prosocial vs. control", path = "../figures/h_tc_lik_get_test_prosoc_check.tex")
format_latex(h_tc_lik_get_test_selfint_check, dv_label, trt_label = "Self-interest vs. control", path = "../figures/h_tc_lik_get_test_selfint_check.tex")
format_latex(h_tt_lik_get_test_check, dv_label, trt_label = "Prosocial vs. self-interest", path = "../figures/h_tt_lik_get_test_check.tex")



## likelihood of reporting positive test to app

# define vars
dv <- "app_submit_positive_result_std.2"
dv_pre <- "app_submit_positive_result.1"
D <- "treat_comp.1"
dv_label <- "Reporting positive test"

# run models
h_tc_lik_rep_result_pooled <- run_models(trt = "treat_any.1", dv, dv_pre, covars, D, ger_df_wide)
h_tc_lik_rep_result_prosoc <- run_models(trt = "treat_prosoc.1", dv, dv_pre, covars, D, filter(ger_df_wide, treat.1 != "selfinterest"))
h_tc_lik_rep_result_selfint <- run_models(trt = "treat_selfint.1", dv, dv_pre, covars, D, filter(ger_df_wide, treat.1 != "prosocial"))
h_tt_lik_rep_result <- run_models(trt = "treat_prosoc.1", dv, dv_pre, covars, D, filter(ger_df_wide, treat.1 != "control"))

# run models, only with people in treatment groups who pass manipulation check
h_tc_lik_rep_result_pooled_check <- run_models(trt = "treat_any.1", dv, dv_pre, covars, D, filter(ger_df_wide, manipcheck_pass.1 == TRUE))
h_tc_lik_rep_result_prosoc_check <- run_models(trt = "treat_prosoc.1", dv, dv_pre, covars, D, filter(ger_df_wide, treat.1 != "selfinterest", manipcheck_pass.1 == TRUE))
h_tc_lik_rep_result_selfint_check <- run_models(trt = "treat_selfint.1", dv, dv_pre, covars, D, filter(ger_df_wide, treat.1 != "prosocial", manipcheck_pass.1 == TRUE))
h_tt_lik_rep_result_check <- run_models(trt = "treat_prosoc.1", dv, dv_pre, covars, D, filter(ger_df_wide, treat.1 != "control", manipcheck_pass.1 == TRUE))

# export results tables
format_latex(h_tc_lik_rep_result_pooled, dv_label, trt_label = "Pooled vs. control", path = "../figures/h_tc_lik_rep_result_pooled.tex")
format_latex(h_tc_lik_rep_result_prosoc, dv_label, trt_label = "Prosocial vs. control", path = "../figures/h_tc_lik_rep_result_prosoc.tex")
format_latex(h_tc_lik_rep_result_selfint, dv_label, trt_label = "Self-interest vs. control", path = "../figures/h_tc_lik_rep_result_selfint.tex")
format_latex(h_tt_lik_rep_result, dv_label, trt_label = "Prosocial vs. self-interest", path = "../figures/h_tt_lik_rep_result.tex")
format_latex(h_tc_lik_rep_result_pooled_check, dv_label, trt_label = "Pooled vs. control", path = "../figures/h_tc_lik_rep_result_pooled_check.tex")
format_latex(h_tc_lik_rep_result_prosoc_check, dv_label, trt_label = "Prosocial vs. control", path = "../figures/h_tc_lik_rep_result_prosoc_check.tex")
format_latex(h_tc_lik_rep_result_selfint_check, dv_label, trt_label = "Self-interest vs. control", path = "../figures/h_tc_lik_rep_result_selfint_check.tex")
format_latex(h_tt_lik_rep_result_check, dv_label, trt_label = "Prosocial vs. self-interest", path = "../figures/h_tt_lik_rep_result_check.tex")





### Test of main hypotheses with W3 outcomes (not pre-registered) ----------------------

# add treatment indicator of incentivization treatment
ger_df_wide$treat_incent_any_global.2 <- ger_df_wide$treat_incent_any.2
ger_df_wide$treat_incent_any_global.2[is.na(ger_df_wide$treat_incent_any_global.2 )] <- FALSE
covars_w3 <- c(covars, "treat_incent_any_global.2")
covars_red_w3 <- c(covars_red, "treat_incent_any_global.2")
  
### H-TC-1a: Subjects assigned to any of the treatments will be more likely to be observed to install the app than subjects assigned to the control group.
### only surveytracking sample

# define vars
dv <- "app_track_installed_before_w3_std"
dv_pre <- "app_track_installed_before_w1"
D <- "treat_comp.1"
dv_label <- "Uptake (tracked)"
dat_surveytracking <- filter(ger_df_wide, sample_type.1 == "surveytracking")

# run models
h_tc_1a_pooled_w3 <- run_models(trt = "treat_any.1", dv, dv_pre, covars_w3, D, dat_surveytracking)
h_tc_1a_prosoc_w3 <- run_models(trt = "treat_prosoc.1", dv, dv_pre, covars_w3, D, filter(dat_surveytracking, treat.1 != "selfinterest"))
h_tc_1a_selfint_w3 <- run_models(trt = "treat_selfint.1", dv, dv_pre, covars_w3, D, filter(dat_surveytracking, treat.1 != "prosocial"))
h_tt_1a_w3 <- run_models(trt = "treat_prosoc.1", dv, dv_pre, covars_w3, D, filter(dat_surveytracking, treat.1 != "control"))

# run models, only with people in treatment groups who pass manipulation check
h_tc_1a_pooled_w3_check <- run_models(trt = "treat_any.1", dv, dv_pre, covars_w3, D, filter(dat_surveytracking, manipcheck_pass.1 == TRUE))
h_tc_1a_prosoc_w3_check <- run_models(trt = "treat_prosoc.1", dv, dv_pre, covars_w3, D, filter(dat_surveytracking, treat.1 != "selfinterest", manipcheck_pass.1 == TRUE))
h_tc_1a_selfint_w3_check <- run_models(trt = "treat_selfint.1", dv, dv_pre, covars_w3, D, filter(dat_surveytracking, treat.1 != "prosocial", manipcheck_pass.1 == TRUE))
h_tt_1a_w3_check <- run_models(trt = "treat_prosoc.1", dv, dv_pre, covars_w3, D, filter(dat_surveytracking, treat.1 != "control", manipcheck_pass.1 == TRUE))

# export results tables
format_latex(h_tc_1a_pooled_w3, dv_label, trt_label = "Pooled vs. control", path = "../figures/h_tc_1a_pooled_w3.tex")
format_latex(h_tc_1a_prosoc_w3, dv_label, trt_label = "Prosocial vs. control", path = "../figures/h_tc_1a_prosoc_w3.tex")
format_latex(h_tc_1a_selfint_w3, dv_label, trt_label = "Self-interest vs. control", path = "../figures/h_tc_1a_selfint_w3.tex")
format_latex(h_tt_1a_w3, dv_label, trt_label = "Prosocial vs. self-interest", path = "../figures/h_tt_1a_w3.tex")
format_latex(h_tc_1a_pooled_w3_check, dv_label, trt_label = "Pooled vs. control", path = "../figures/h_tc_1a_pooled_w3_check.tex")
format_latex(h_tc_1a_prosoc_w3_check, dv_label, trt_label = "Prosocial vs. control", path = "../figures/h_tc_1a_prosoc_w3_check.tex")
format_latex(h_tc_1a_selfint_w3_check, dv_label, trt_label = "Self-interest vs. control", path = "../figures/h_tc_1a_selfint_w3_check.tex")
format_latex(h_tt_1a_w3_check, dv_label, trt_label = "Prosocial vs. self-interest", path = "../figures/h_tt_1a_w3_check.tex")



### H-TC-1b: Subjects assigned to any of the treatments will be more likely to be observed to install the app than subjects assigned to the control group.
### surveytracking + trackingonly sample; reduced set of controls

# define vars
dv <- "app_track_installed_before_w3_std"
dv_pre <- "app_track_installed_before_w1"
D <- "treat_comp.1"
dv_label <- "Uptake (tracked)"
dat_surveytrackingplus <- filter(ger_df_wide, sample_type.1 == "surveytracking" | sample_type.1 == "trackingonly")

# run models
h_tc_1b_pooled_w3 <- run_models(trt = "treat_any.1", dv, dv_pre, covars_red_w3, D, dat_surveytrackingplus)
h_tc_1b_prosoc_w3 <- run_models(trt = "treat_prosoc.1", dv, dv_pre, covars_red_w3, D, filter(dat_surveytrackingplus, treat.1 != "selfinterest"))
h_tc_1b_selfint_w3 <- run_models(trt = "treat_selfint.1", dv, dv_pre, covars_red_w3, D, filter(dat_surveytrackingplus, treat.1 != "prosocial"))
h_tt_1b_w3 <- run_models(trt = "treat_prosoc.1", dv, dv_pre, covars_red_w3, D, filter(dat_surveytrackingplus, treat.1 != "control"))

# run models, only with people in treatment groups who pass manipulation check
h_tc_1b_pooled_w3_check <- run_models(trt = "treat_any.1", dv, dv_pre, covars_red_w3, D, filter(dat_surveytrackingplus, manipcheck_pass.1 == TRUE))
h_tc_1b_prosoc_w3_check <- run_models(trt = "treat_prosoc.1", dv, dv_pre, covars_red_w3, D, filter(dat_surveytrackingplus, treat.1 != "selfinterest", manipcheck_pass.1 == TRUE))
h_tc_1b_selfint_w3_check <- run_models(trt = "treat_selfint.1", dv, dv_pre, covars_red_w3, D, filter(dat_surveytrackingplus, treat.1 != "prosocial", manipcheck_pass.1 == TRUE))
h_tt_1b_w3_check <- run_models(trt = "treat_prosoc.1", dv, dv_pre, covars_red_w3, D, filter(dat_surveytrackingplus, treat.1 != "control", manipcheck_pass.1 == TRUE))


# export results tables
format_latex(h_tc_1b_pooled_w3, dv_label, trt_label = "Pooled vs. control", path = "../figures/h_tc_1b_pooled_w3.tex")
format_latex(h_tc_1b_prosoc_w3, dv_label, trt_label = "Prosocial vs. control", path = "../figures/h_tc_1b_prosoc_w3.tex")
format_latex(h_tc_1b_selfint_w3, dv_label, trt_label = "Self-interest vs. control", path = "../figures/h_tc_1b_selfint_w3.tex")
format_latex(h_tt_1b_w3, dv_label, trt_label = "Prosocial vs. self-interest", path = "../figures/h_tt_1b_w3.tex")
format_latex(h_tc_1b_pooled_w3_check, dv_label, trt_label = "Pooled vs. control", path = "../figures/h_tc_1b_pooled_w3_check.tex")
format_latex(h_tc_1b_prosoc_w3_check, dv_label, trt_label = "Prosocial vs. control", path = "../figures/h_tc_1b_prosoc_w3_check.tex")
format_latex(h_tc_1b_selfint_w3_check, dv_label, trt_label = "Self-interest vs. control", path = "../figures/h_tc_1b_selfint_w3_check.tex")
format_latex(h_tt_1b_w3_check, dv_label, trt_label = "Prosocial vs. self-interest", path = "../figures/h_tt_1b_w3_check.tex")



### H-TC-1c: Subjects assigned to any of the treatments will be more likely to be observed to install the app than subjects assigned to the control group.
### surveytracking + surveyonly + trackingonly sample; hybrid outcome measure; reduced set of controls

# define vars
dv <- "app_hyb_installed_before_w3_std"
dv_pre <- "app_hyb_installed_before_w1"
D <- "treat_comp.1"
dv_label <- "Uptake (hybrid)"

# run models
h_tc_1c_pooled_w3 <- run_models(trt = "treat_any.1", dv, dv_pre, covars_red_w3, D, ger_df_wide)
h_tc_1c_prosoc_w3 <- run_models(trt = "treat_prosoc.1", dv, dv_pre, covars_red_w3, D, filter(ger_df_wide, treat.1 != "selfinterest"))
h_tc_1c_selfint_w3 <- run_models(trt = "treat_selfint.1", dv, dv_pre, covars_red_w3, D, filter(ger_df_wide, treat.1 != "prosocial"))
h_tt_1c_w3 <- run_models(trt = "treat_prosoc.1", dv, dv_pre, covars_red_w3, D, filter(ger_df_wide, treat.1 != "control"))

# run models, only with people in treatment groups who pass manipulation check
h_tc_1c_pooled_w3_check <- run_models(trt = "treat_any.1", dv, dv_pre, covars_red_w3, D, filter(ger_df_wide, manipcheck_pass.1 == TRUE))
h_tc_1c_prosoc_w3_check <- run_models(trt = "treat_prosoc.1", dv, dv_pre, covars_red_w3, D, filter(ger_df_wide, treat.1 != "selfinterest", manipcheck_pass.1 == TRUE))
h_tc_1c_selfint_w3_check <- run_models(trt = "treat_selfint.1", dv, dv_pre, covars_red_w3, D, filter(ger_df_wide, treat.1 != "prosocial", manipcheck_pass.1 == TRUE))
h_tt_1c_w3_check <- run_models(trt = "treat_prosoc.1", dv, dv_pre, covars_red_w3, D, filter(ger_df_wide, treat.1 != "control", manipcheck_pass.1 == TRUE))

# export results tables
format_latex(h_tc_1c_pooled_w3, dv_label, trt_label = "Pooled vs. control", path = "../figures/h_tc_1c_pooled_w3.tex")
format_latex(h_tc_1c_prosoc_w3, dv_label, trt_label = "Prosocial vs. control", path = "../figures/h_tc_1c_prosoc_w3.tex")
format_latex(h_tc_1c_selfint_w3, dv_label, trt_label = "Self-interest vs. control", path = "../figures/h_tc_1c_selfint_w3.tex")
format_latex(h_tt_1c_w3, dv_label, trt_label = "Prosocial vs. self-interest", path = "../figures/h_tt_1c_w3.tex")
format_latex(h_tc_1c_pooled_w3_check, dv_label, trt_label = "Pooled vs. control", path = "../figures/h_tc_1c_pooled_w3_check.tex")
format_latex(h_tc_1c_prosoc_w3_check, dv_label, trt_label = "Prosocial vs. control", path = "../figures/h_tc_1c_prosoc_w3_check.tex")
format_latex(h_tc_1c_selfint_w3_check, dv_label, trt_label = "Self-interest vs. control", path = "../figures/h_tc_1c_selfint_w3_check.tex")
format_latex(h_tt_1c_w3_check, dv_label, trt_label = "Prosocial vs. self-interest", path = "../figures/h_tt_1c_w3_check.tex")



### H-TC-2: Subjects assigned to any of the treatments will be more likely to report using the app than subjects assigned to the control group.

# define vars
dv <- "app_rep_installed_before_w2_std"
dv_pre <- "app_rep_installed_before_w1"
D <- "treat_comp.1"
dv_label <- "Uptake (reported)"

# run models
h_tc_2_pooled_w3 <- run_models(trt = "treat_any.1", dv, dv_pre, covars_w3, D, ger_df_wide)
h_tc_2_prosoc_w3 <- run_models(trt = "treat_prosoc.1", dv, dv_pre, covars_w3, D, filter(ger_df_wide, treat.1 != "selfinterest"))
h_tc_2_selfint_w3 <- run_models(trt = "treat_selfint.1", dv, dv_pre, covars_w3, D, filter(ger_df_wide, treat.1 != "prosocial"))
h_tt_2_w3 <- run_models(trt = "treat_prosoc.1", dv, dv_pre, covars_w3, D, filter(ger_df_wide, treat.1 != "control"))

# run models, only with people in treatment groups who pass manipulation check
h_tc_2_pooled_w3_check <- run_models(trt = "treat_any.1", dv, dv_pre, covars_w3, D, filter(ger_df_wide, manipcheck_pass.1 == TRUE))
h_tc_2_prosoc_w3_check <- run_models(trt = "treat_prosoc.1", dv, dv_pre, covars_w3, D, filter(ger_df_wide, treat.1 != "selfinterest", manipcheck_pass.1 == TRUE))
h_tc_2_selfint_w3_check <- run_models(trt = "treat_selfint.1", dv, dv_pre, covars_w3, D, filter(ger_df_wide, treat.1 != "prosocial", manipcheck_pass.1 == TRUE))
h_tt_2_w3_check <- run_models(trt = "treat_prosoc.1", dv, dv_pre, covars_w3, D, filter(ger_df_wide, treat.1 != "control", manipcheck_pass.1 == TRUE))

# export results tables
format_latex(h_tc_2_pooled_w3, dv_label, trt_label = "Pooled vs. control", path = "../figures/h_tc_2_pooled_w3.tex")
format_latex(h_tc_2_prosoc_w3, dv_label, trt_label = "Prosocial vs. control", path = "../figures/h_tc_2_prosoc_w3.tex")
format_latex(h_tc_2_selfint_w3, dv_label, trt_label = "Self-interest vs. control", path = "../figures/h_tc_2_selfint_w3.tex")
format_latex(h_tt_2_w3, dv_label, trt_label = "Prosocial vs. self-interest", path = "../figures/h_tt_2_w3.tex")
format_latex(h_tc_2_pooled_w3_check, dv_label, trt_label = "Pooled vs. control", path = "../figures/h_tc_2_pooled_w3_check.tex")
format_latex(h_tc_2_prosoc_w3_check, dv_label, trt_label = "Prosocial vs. control", path = "../figures/h_tc_2_prosoc_w3_check.tex")
format_latex(h_tc_2_selfint_w3_check, dv_label, trt_label = "Self-interest vs. control", path = "../figures/h_tc_2_selfint_w3_check.tex")
format_latex(h_tt_2_w3_check, dv_label, trt_label = "Prosocial vs. self-interest", path = "../figures/h_tt_2_w3_check.tex")



### H-TC-3: Subjects assigned to any of the treatments will be more likely to report having their smartphone’s bluetooth function activated than subjects assigned to the control group.

# define vars
dv <- "app_bluetooth_act_std.3"
dv_pre <- "app_bluetooth_act.1"
D <- "treat_comp.1"
dv_label <- "Bluetooth active (reported)"

# run models
h_tc_3_pooled_w3 <- run_models(trt = "treat_any.1", dv, dv_pre, covars_w3, D, ger_df_wide)
h_tc_3_prosoc_w3 <- run_models(trt = "treat_prosoc.1", dv, dv_pre, covars_w3, D, filter(ger_df_wide, treat.1 != "selfinterest"))
h_tc_3_selfint_w3 <- run_models(trt = "treat_selfint.1", dv, dv_pre, covars_w3, D, filter(ger_df_wide, treat.1 != "prosocial"))
h_tt_3_w3 <- run_models(trt = "treat_prosoc.1", dv, dv_pre, covars_w3, D, filter(ger_df_wide, treat.1 != "control"))

# run models, only with people in treatment groups who pass manipulation check
h_tc_3_pooled_w3_check <- run_models(trt = "treat_any.1", dv, dv_pre, covars_w3, D, filter(ger_df_wide, manipcheck_pass.1 == TRUE))
h_tc_3_prosoc_w3_check <- run_models(trt = "treat_prosoc.1", dv, dv_pre, covars_w3, D, filter(ger_df_wide, treat.1 != "selfinterest", manipcheck_pass.1 == TRUE))
h_tc_3_selfint_w3_check <- run_models(trt = "treat_selfint.1", dv, dv_pre, covars_w3, D, filter(ger_df_wide, treat.1 != "prosocial", manipcheck_pass.1 == TRUE))
h_tt_3_w3_check <- run_models(trt = "treat_prosoc.1", dv, dv_pre, covars_w3, D, filter(ger_df_wide, treat.1 != "control", manipcheck_pass.1 == TRUE))

# export results tables
format_latex(h_tc_3_pooled_w3, dv_label, trt_label = "Pooled vs. control", path = "../figures/h_tc_3_pooled_w3.tex")
format_latex(h_tc_3_prosoc_w3, dv_label, trt_label = "Prosocial vs. control", path = "../figures/h_tc_3_prosoc_w3.tex")
format_latex(h_tc_3_selfint_w3, dv_label, trt_label = "Self-interest vs. control", path = "../figures/h_tc_3_selfint_w3.tex")
format_latex(h_tt_3_w3, dv_label, trt_label = "Prosocial vs. self-interest", path = "../figures/h_tt_3_w3.tex")
format_latex(h_tc_3_pooled_w3_check, dv_label, trt_label = "Pooled vs. control", path = "../figures/h_tc_3_pooled_w3_check.tex")
format_latex(h_tc_3_prosoc_w3_check, dv_label, trt_label = "Prosocial vs. control", path = "../figures/h_tc_3_prosoc_w3_check.tex")
format_latex(h_tc_3_selfint_w3_check, dv_label, trt_label = "Self-interest vs. control", path = "../figures/h_tc_3_selfint_w3_check.tex")
format_latex(h_tt_3_w3_check, dv_label, trt_label = "Prosocial vs. self-interest", path = "../figures/h_tt_3_w3_check.tex")



### H-TC-4: Subjects assigned to any of the treatments will report a higher likelihood of installing the app in the future than subjects assigned to the control group. 

dv <- "app_likely_install_std.3"
dv_pre <- "app_likely_install.1"
D <- "treat_comp.1"
dv_label <- "Likelihood to install"

# run models
h_tc_4_pooled_w3 <- run_models(trt = "treat_any.1", dv, dv_pre, covars_w3, D, ger_df_wide)
h_tc_4_prosoc_w3 <- run_models(trt = "treat_prosoc.1", dv, dv_pre, covars_w3, D, filter(ger_df_wide, treat.1 != "selfinterest"))
h_tc_4_selfint_w3 <- run_models(trt = "treat_selfint.1", dv, dv_pre, covars_w3, D, filter(ger_df_wide, treat.1 != "prosocial"))
h_tt_4_w3 <- run_models(trt = "treat_prosoc.1", dv, dv_pre, covars_w3, D, filter(ger_df_wide, treat.1 != "control"))

# run models, only with people in treatment groups who pass manipulation check
h_tc_4_pooled_w3_check <- run_models(trt = "treat_any.1", dv, dv_pre, covars_w3, D, filter(ger_df_wide, manipcheck_pass.1 == TRUE))
h_tc_4_prosoc_w3_check <- run_models(trt = "treat_prosoc.1", dv, dv_pre, covars_w3, D, filter(ger_df_wide, treat.1 != "selfinterest", manipcheck_pass.1 == TRUE))
h_tc_4_selfint_w3_check <- run_models(trt = "treat_selfint.1", dv, dv_pre, covars_w3, D, filter(ger_df_wide, treat.1 != "prosocial", manipcheck_pass.1 == TRUE))
h_tt_4_w3_check <- run_models(trt = "treat_prosoc.1", dv, dv_pre, covars_w3, D, filter(ger_df_wide, treat.1 != "control", manipcheck_pass.1 == TRUE))


# export results tables
format_latex(h_tc_4_pooled_w3, dv_label, trt_label = "Pooled vs. control", path = "../figures/h_tc_4_pooled_w3.tex")
format_latex(h_tc_4_prosoc_w3, dv_label, trt_label = "Prosocial vs. control", path = "../figures/h_tc_4_prosoc_w3.tex")
format_latex(h_tc_4_selfint_w3, dv_label, trt_label = "Self-interest vs. control", path = "../figures/h_tc_4_selfint_w3.tex")
format_latex(h_tt_4_w3, dv_label, trt_label = "Prosocial vs. self-interest", path = "../figures/h_tt_4_w3.tex")
format_latex(h_tc_4_pooled_w3_check, dv_label, trt_label = "Pooled vs. control", path = "../figures/h_tc_4_pooled_w3_check.tex")
format_latex(h_tc_4_prosoc_w3_check, dv_label, trt_label = "Prosocial vs. control", path = "../figures/h_tc_4_prosoc_w3_check.tex")
format_latex(h_tc_4_selfint_w3_check, dv_label, trt_label = "Self-interest vs. control", path = "../figures/h_tc_4_selfint_w3_check.tex")
format_latex(h_tt_4_w3_check, dv_label, trt_label = "Prosocial vs. self-interest", path = "../figures/h_tt_4_w3_check.tex")




### H-TC-5: Subjects assigned to any of the treatments will show higher levels of knowledge about the app than subjects assigned to the control group.

# define vars
dv <- "app_know_index_std.3" # app_know_index_std.1 is post-treatment and only used as manipulation check
dv_pre <- NULL
D <- "treat_comp.1"
dv_label <- "Knowledge"

# run models
h_tc_5_pooled_w3 <- run_models(trt = "treat_any.1", dv, dv_pre, covars_w3, D, ger_df_wide)
h_tc_5_prosoc_w3 <- run_models(trt = "treat_prosoc.1", dv, dv_pre, covars_w3, D, filter(ger_df_wide, treat.1 != "selfinterest"))
h_tc_5_selfint_w3 <- run_models(trt = "treat_selfint.1", dv, dv_pre, covars_w3, D, filter(ger_df_wide, treat.1 != "prosocial"))
h_tt_5_w3 <- run_models(trt = "treat_prosoc.1", dv, dv_pre, covars_w3, D, filter(ger_df_wide, treat.1 != "control"))

# run models, only with people in treatment groups who pass manipulation check
h_tc_5_pooled_w3_check <- run_models(trt = "treat_any.1", dv, dv_pre, covars_w3, D, filter(ger_df_wide, manipcheck_pass.1 == TRUE))
h_tc_5_prosoc_w3_check <- run_models(trt = "treat_prosoc.1", dv, dv_pre, covars_w3, D, filter(ger_df_wide, treat.1 != "selfinterest", manipcheck_pass.1 == TRUE))
h_tc_5_selfint_w3_check <- run_models(trt = "treat_selfint.1", dv, dv_pre, covars_w3, D, filter(ger_df_wide, treat.1 != "prosocial", manipcheck_pass.1 == TRUE))
h_tt_5_w3_check <- run_models(trt = "treat_prosoc.1", dv, dv_pre, covars_w3, D, filter(ger_df_wide, treat.1 != "control", manipcheck_pass.1 == TRUE))

# export results tables
format_latex(h_tc_5_pooled_w3, dv_label, trt_label = "Pooled vs. control", path = "../figures/h_tc_5_pooled_w3.tex")
format_latex(h_tc_5_prosoc_w3, dv_label, trt_label = "Prosocial vs. control", path = "../figures/h_tc_5_prosoc_w3.tex")
format_latex(h_tc_5_selfint_w3, dv_label, trt_label = "Self-interest vs. control", path = "../figures/h_tc_5_selfint_w3.tex")
format_latex(h_tt_5_w3, dv_label, trt_label = "Prosocial vs. self-interest", path = "../figures/h_tt_5_w3.tex")
format_latex(h_tc_5_pooled_w3_check, dv_label, trt_label = "Pooled vs. control", path = "../figures/h_tc_5_pooled_w3_check.tex")
format_latex(h_tc_5_prosoc_w3_check, dv_label, trt_label = "Prosocial vs. control", path = "../figures/h_tc_5_prosoc_w3_check.tex")
format_latex(h_tc_5_selfint_w3_check, dv_label, trt_label = "Self-interest vs. control", path = "../figures/h_tc_5_selfint_w3_check.tex")
format_latex(h_tt_5_w3_check, dv_label, trt_label = "Prosocial vs. self-interest", path = "../figures/h_tt_5_w3_check.tex")



### H-TC-6: Subjects assigned to any of the treatments will report more positive attitudes towards the app than subjects assigned to the control group.

# define vars
dv <- "att_scale_score_std.3"
dv_pre <- "att_scale_score.1"
D <- "treat_comp.1"
dv_label <- "Positive attitudes"

# run models
h_tc_6_pooled_w3 <- run_models(trt = "treat_any.1", dv, dv_pre, covars_w3, D, ger_df_wide)
h_tc_6_prosoc_w3 <- run_models(trt = "treat_prosoc.1", dv, dv_pre, covars_w3, D, filter(ger_df_wide, treat.1 != "selfinterest"))
h_tc_6_selfint_w3 <- run_models(trt = "treat_selfint.1", dv, dv_pre, covars_w3, D, filter(ger_df_wide, treat.1 != "prosocial"))
h_tt_6_w3 <- run_models(trt = "treat_prosoc.1", dv, dv_pre, covars_w3, D, filter(ger_df_wide, treat.1 != "control"))

# run models, only with people in treatment groups who pass manipulation check
h_tc_6_pooled_w3_check <- run_models(trt = "treat_any.1", dv, dv_pre, covars_w3, D, filter(ger_df_wide, manipcheck_pass.1 == TRUE))
h_tc_6_prosoc_w3_check <- run_models(trt = "treat_prosoc.1", dv, dv_pre, covars_w3, D, filter(ger_df_wide, treat.1 != "selfinterest", manipcheck_pass.1 == TRUE))
h_tc_6_selfint_w3_check <- run_models(trt = "treat_selfint.1", dv, dv_pre, covars_w3, D, filter(ger_df_wide, treat.1 != "prosocial", manipcheck_pass.1 == TRUE))
h_tt_6_w3_check <- run_models(trt = "treat_prosoc.1", dv, dv_pre, covars_w3, D, filter(ger_df_wide, treat.1 != "control", manipcheck_pass.1 == TRUE))

# export results tables
format_latex(h_tc_6_pooled_w3, dv_label, trt_label = "Pooled vs. control", path = "../figures/h_tc_6_pooled_w3.tex")
format_latex(h_tc_6_prosoc_w3, dv_label, trt_label = "Prosocial vs. control", path = "../figures/h_tc_6_prosoc_w3.tex")
format_latex(h_tc_6_selfint_w3, dv_label, trt_label = "Self-interest vs. control", path = "../figures/h_tc_6_selfint_w3.tex")
format_latex(h_tt_6_w3, dv_label, trt_label = "Prosocial vs. self-interest", path = "../figures/h_tt_6_w3.tex")
format_latex(h_tc_6_pooled_w3_check, dv_label, trt_label = "Pooled vs. control", path = "../figures/h_tc_6_pooled_w3_check.tex")
format_latex(h_tc_6_prosoc_w3_check, dv_label, trt_label = "Prosocial vs. control", path = "../figures/h_tc_6_prosoc_w3_check.tex")
format_latex(h_tc_6_selfint_w3_check, dv_label, trt_label = "Self-interest vs. control", path = "../figures/h_tc_6_selfint_w3_check.tex")
format_latex(h_tt_6_w3_check, dv_label, trt_label = "Prosocial vs. self-interest", path = "../figures/h_tt_6_w3_check.tex")



## likelihood of getting tested/quarantined after warning

# define vars
dv <- "app_test_warning_std.3"
dv_pre <- "app_test_warning.1"
D <- "treat_comp.1"
dv_label <- "Tested after warning"

# run models
h_tc_lik_get_test_pooled_w3 <- run_models(trt = "treat_any.1", dv, dv_pre, covars, D, ger_df_wide)
h_tc_lik_get_test_prosoc_w3 <- run_models(trt = "treat_prosoc.1", dv, dv_pre, covars, D, filter(ger_df_wide, treat.1 != "selfinterest"))
h_tc_lik_get_test_selfint_w3 <- run_models(trt = "treat_selfint.1", dv, dv_pre, covars, D, filter(ger_df_wide, treat.1 != "prosocial"))
h_tt_get_test_w3 <- run_models(trt = "treat_prosoc.1", dv, dv_pre, covars, D, filter(ger_df_wide, treat.1 != "control"))

# run models, only with people in treatment groups who pass manipulation check
h_tc_lik_get_test_pooled_w3_check <- run_models(trt = "treat_any.1", dv, dv_pre, covars, D, filter(ger_df_wide, manipcheck_pass.1 == TRUE))
h_tc_lik_get_test_prosoc_w3_check <- run_models(trt = "treat_prosoc.1", dv, dv_pre, covars, D, filter(ger_df_wide, treat.1 != "selfinterest", manipcheck_pass.1 == TRUE))
h_tc_lik_get_test_selfint_w3_check <- run_models(trt = "treat_selfint.1", dv, dv_pre, covars, D, filter(ger_df_wide, treat.1 != "prosocial", manipcheck_pass.1 == TRUE))
h_tt_lik_get_test_w3_check <- run_models(trt = "treat_prosoc.1", dv, dv_pre, covars, D, filter(ger_df_wide, treat.1 != "control", manipcheck_pass.1 == TRUE))

# export results tables
format_latex(h_tc_lik_get_test_pooled_w3, dv_label, trt_label = "Pooled vs. control", path = "../figures/h_tc_lik_get_test_pooled_w3.tex")
format_latex(h_tc_lik_get_test_prosoc_w3, dv_label, trt_label = "Prosocial vs. control", path = "../figures/h_tc_lik_get_test_prosoc_w3.tex")
format_latex(h_tc_lik_get_test_selfint_w3, dv_label, trt_label = "Self-interest vs. control", path = "../figures/h_tc_lik_get_test_selfint_w3.tex")
format_latex(h_tt_get_test_w3, dv_label, trt_label = "Prosocial vs. self-interest", path = "../figures/h_tt_lik_get_test_w3.tex")
format_latex(h_tc_lik_get_test_pooled_w3_check, dv_label, trt_label = "Pooled vs. control", path = "../figures/h_tc_lik_get_test_pooled_w3_check.tex")
format_latex(h_tc_lik_get_test_prosoc_w3_check, dv_label, trt_label = "Prosocial vs. control", path = "../figures/h_tc_lik_get_test_prosoc_w3_check.tex")
format_latex(h_tc_lik_get_test_selfint_w3_check, dv_label, trt_label = "Self-interest vs. control", path = "../figures/h_tc_lik_get_test_selfint_w3_check.tex")
format_latex(h_tt_lik_get_test_w3_check, dv_label, trt_label = "Prosocial vs. self-interest", path = "../figures/h_tt_lik_get_test_w3_check.tex")



## likelihood of reporting positive test to app

# define vars
dv <- "app_submit_positive_result_std.3"
dv_pre <- "app_submit_positive_result.1"
D <- "treat_comp.1"
dv_label <- "Reporting positive test"

# run models
h_tc_lik_rep_result_pooled_w3 <- run_models(trt = "treat_any.1", dv, dv_pre, covars, D, ger_df_wide)
h_tc_lik_rep_result_prosoc_w3 <- run_models(trt = "treat_prosoc.1", dv, dv_pre, covars, D, filter(ger_df_wide, treat.1 != "selfinterest"))
h_tc_lik_rep_result_selfint_w3 <- run_models(trt = "treat_selfint.1", dv, dv_pre, covars, D, filter(ger_df_wide, treat.1 != "prosocial"))
h_tt_lik_rep_result_w3 <- run_models(trt = "treat_prosoc.1", dv, dv_pre, covars, D, filter(ger_df_wide, treat.1 != "control"))

# run models, only with people in treatment groups who pass manipulation check
h_tc_lik_rep_result_pooled_w3_check <- run_models(trt = "treat_any.1", dv, dv_pre, covars, D, filter(ger_df_wide, manipcheck_pass.1 == TRUE))
h_tc_lik_rep_result_prosoc_w3_check <- run_models(trt = "treat_prosoc.1", dv, dv_pre, covars, D, filter(ger_df_wide, treat.1 != "selfinterest", manipcheck_pass.1 == TRUE))
h_tc_lik_rep_result_selfint_w3_check <- run_models(trt = "treat_selfint.1", dv, dv_pre, covars, D, filter(ger_df_wide, treat.1 != "prosocial", manipcheck_pass.1 == TRUE))
h_tt_lik_rep_result_w3_check <- run_models(trt = "treat_prosoc.1", dv, dv_pre, covars, D, filter(ger_df_wide, treat.1 != "control", manipcheck_pass.1 == TRUE))

# export results tables
format_latex(h_tc_lik_rep_result_pooled_w3, dv_label, trt_label = "Pooled vs. control", path = "../figures/h_tc_lik_rep_result_pooled_w3.tex")
format_latex(h_tc_lik_rep_result_prosoc_w3, dv_label, trt_label = "Prosocial vs. control", path = "../figures/h_tc_lik_rep_result_prosoc_w3.tex")
format_latex(h_tc_lik_rep_result_selfint_w3, dv_label, trt_label = "Self-interest vs. control", path = "../figures/h_tc_lik_rep_result_selfint_w3.tex")
format_latex(h_tt_lik_rep_result_w3, dv_label, trt_label = "Prosocial vs. self-interest", path = "../figures/h_tt_lik_rep_result_w3.tex")
format_latex(h_tc_lik_rep_result_pooled_w3_check, dv_label, trt_label = "Pooled vs. control", path = "../figures/h_tc_lik_rep_result_pooled_w3_check.tex")
format_latex(h_tc_lik_rep_result_prosoc_w3_check, dv_label, trt_label = "Prosocial vs. control", path = "../figures/h_tc_lik_rep_result_prosoc_w3_check.tex")
format_latex(h_tc_lik_rep_result_selfint_w3_check, dv_label, trt_label = "Self-interest vs. control", path = "../figures/h_tc_lik_rep_result_selfint_w3_check.tex")
format_latex(h_tt_lik_rep_result_w3_check, dv_label, trt_label = "Prosocial vs. self-interest", path = "../figures/h_tt_lik_rep_result_w3_check.tex")







### Test of heterogeneity hypotheses ----------------------

dv_vec <- c("app_track_installed_before_w2_std", "app_hyb_installed_before_w2_std", "app_rep_installed_before_w2_std", "app_know_index_std.2", "att_scale_score_std.2", "app_sharing_clicks_std.1", "app_info_clicks_std.1", "app_bluetooth_act_std.2", "app_test_warning_std.2", "app_submit_positive_result_std.2")
dv_pre_vec <- c("app_track_installed_before_w1", "app_hyb_installed_before_w1", "app_rep_installed_before_w1", NA, "att_scale_score_std.1", NA, NA, "app_bluetooth_act.1", "app_test_warning.1", "app_submit_positive_result.1")
dv_labels <- c("Uptake (tracked)", "Uptake (hybrid)", "Uptake (reported)", "Knowledge", "Positive attitudes", "Message sharing (tracked)", "Information lookup (tracked)", "Bluetooth active (reported)", "Tested after warning", "Reporting positive test")

### H-TT-1: Subjects with high levels of social responsibility will show higher treatment effects in the pro-social treatment than subjects with low levels of social responsibility.

het_prosoc_list <- list()
het_prosoc_effect <- character()
for (i in seq_along(dv_vec)){
  dv <- dv_vec[i]
  dv_pre <- dv_pre_vec[i]
  if(is.na(dv_pre)){ dv_pre <- NULL}
  het_prosoc_list[[i]] <- try(heterogeneous_itt(dv = dv, dv_pre = dv_pre, covars = covars,
                    moderator = "socresp_score.1", trt = "treat_prosoc.1",
                    data = filter(ger_df_wide, treat.1 != "selfinterest")))
  het_prosoc_effect[i] <- try(het_prosoc_list[[i]][[2]])
}

### H-TT-2: Subjects with high levels of self-interest will show higher treatment effects in the self-interest treatment than subjects with low levels of self-interest.

het_selfint_list <- list()
het_selfint_effect <- character()
for (i in seq_along(dv_vec)){
  dv <- dv_vec[i]
  dv_pre <- dv_pre_vec[i]
  if(is.na(dv_pre)){ dv_pre <- NULL}
  het_selfint_list[[i]] <- try(heterogeneous_itt(dv = dv, dv_pre = dv_pre, covars = covars,
                                                moderator = "selfint_score.1", trt = "treat_selfint.1",
                                                data = filter(ger_df_wide, treat.1 != "prosocial")))
  het_selfint_effect[i] <- try(het_selfint_list[[i]][[2]])
}


### H-App-Het (not pre-registered): Subjects that had not used the app before will show higher treatment effects than subjects who had used the app before.

het_appuse_list <- list()
het_appuse_effect <- character()
for (i in seq_along(dv_vec)){
  dv <- dv_vec[i]
  dv_pre <- dv_pre_vec[i]
  if(is.na(dv_pre)){ dv_pre <- NULL}
  het_appuse_list[[i]] <- try(heterogeneous_itt(dv = dv, dv_pre = dv_pre, covars = covars,
                                                 moderator = "app_hyb_installed_before_w1", trt = "treat_any.1",
                                                 data = ger_df_wide))
  het_appuse_effect[i] <- try(het_appuse_list[[i]][[2]])
  if(str_detect(dv_vec[i], "installed")){
    het_appuse_effect[i] <- ""
  }
}


### H-Age-Het (not pre-registered): What's the moderating effect of age?

het_age_list <- list()
het_age_effect <- character()
for (i in seq_along(dv_vec)){
  dv <- dv_vec[i]
  dv_pre <- dv_pre_vec[i]
  if(is.na(dv_pre)){ dv_pre <- NULL}
  het_age_list[[i]] <- try(heterogeneous_itt(dv = dv, dv_pre = dv_pre, covars = covars,
                                                moderator = "age.1", trt = "treat_any.1",
                                                data = ger_df_wide))
  het_age_effect[i] <- try(het_age_list[[i]][[2]])
}

### H-NPI-Het (not pre-registered): What's the moderating effect of general NPI compliance?

het_npi_list <- list()
het_npi_effect <- character()
for (i in seq_along(dv_vec)){
  dv <- dv_vec[i]
  dv_pre <- dv_pre_vec[i]
  if(is.na(dv_pre)){ dv_pre <- NULL}
  het_npi_list[[i]] <- try(heterogeneous_itt(dv = dv, dv_pre = dv_pre, covars = covars,
                                             moderator = "aha_compliance_cat2.1", trt = "treat_any.1",
                                             data = ger_df_wide))
  het_npi_effect[i] <- try(het_npi_list[[i]][[2]])
}

### H-GovTrust-Het (not pre-registered): What's the moderating effect of trust in government?

het_govtrust_list <- list()
het_govtrust_effect <- character()
for (i in seq_along(dv_vec)){
  dv <- dv_vec[i]
  dv_pre <- dv_pre_vec[i]
  if(is.na(dv_pre)){ dv_pre <- NULL}
  het_govtrust_list[[i]] <- try(heterogeneous_itt(dv = dv, dv_pre = dv_pre, covars = covars,
                                             moderator = "trust_gov_cat2.1", trt = "treat_any.1",
                                             data = ger_df_wide))
  het_govtrust_effect[i] <- try(het_govtrust_list[[i]][[2]])
}


### H-ThreatPerception-Het (not pre-registered): What's the moderating effect of COVID threat perception?

het_threatfamf_list <- list()
het_threatfamf_effect <- character()
for (i in seq_along(dv_vec)){
  dv <- dv_vec[i]
  dv_pre <- dv_pre_vec[i]
  if(is.na(dv_pre)){ dv_pre <- NULL}
  het_threatfamf_list[[i]] <- try(heterogeneous_itt(dv = dv, dv_pre = dv_pre, covars = covars,
                                                  moderator = "covidconcerned_famf_cat2.1", trt = "treat_any.1",
                                                  data = ger_df_wide))
  het_threatfamf_effect[i] <- try(het_threatfamf_list[[i]][[2]])
}


# print to latex
het_effects_df <- 
  data.frame(Outcome = dv_labels,
             Prosoc = het_prosoc_effect,
             Selfint = het_selfint_effect,
             stringsAsFactors = FALSE
  )
print(xtable(het_effects_df), include.rownames = FALSE)


het_effects_df <- 
  data.frame(Outcome = dv_labels,
             Appuse = het_appuse_effect,
             Age = het_age_effect,
             NPI = het_npi_effect,
             GovTrust = het_govtrust_effect,
             stringsAsFactors = FALSE
  )
print(xtable(het_effects_df), include.rownames = FALSE)


