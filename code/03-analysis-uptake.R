### ----------------------------------------
### analysis of app uptake and risk behavior
### ----------------------------------------

# load data --------------------------

ger_df_wide <- readRDS("../data/clean/ger_df_wide_an.RDS")



# probit model of reported app installation (pre-treatment) -------------

form <- as.formula(paste0("app_rep_installed_at_w1 ~ ", paste0(covars_adapted_df$variable, collapse = "+")))
summary(probit_app_report <- glm(form, data = ger_df_wide, family = binomial(link = "probit") ))

form <- as.formula(paste0("app_rep_installed_at_w1 ~ ", paste0(covars_adapted_df$variable[covars_adapted_df$category == "Sociodemographics"], collapse = "+")))
summary(probit_app_report_socdem <- glm(form, data = ger_df_wide, family = binomial(link = "probit") ))

form <- as.formula(paste0("app_rep_installed_at_w1 ~ ", paste0(covars_adapted_df$variable[covars_adapted_df$category == "Risk status and behavior"], collapse = "+")))
summary(probit_app_report_risk <- glm(form, data = ger_df_wide, family = binomial(link = "probit") ))

form <- as.formula(paste0("app_rep_installed_at_w1 ~ ", paste0(covars_adapted_df$variable[covars_adapted_df$category == "Motivational factors"], collapse = "+")))
summary(probit_app_report_motiv <- glm(form, data = ger_df_wide, family = binomial(link = "probit") ))


# probit model of tracked app installation (pre-treatment) -------------
form <- as.formula(paste0("app_track_installed_at_w1 ~ ", paste0(covars_adapted_df$variable, collapse = "+")))
summary(probit_app_tracked <- glm(form, data = filter(ger_df_wide, sample_type.1 == "surveytracking"), family = binomial(link = "probit")))

form <- as.formula(paste0("app_track_installed_at_w1 ~ ", paste0(covars_adapted_df$variable[covars_adapted_df$category == "Sociodemographics"], collapse = "+")))
summary(probit_app_tracked_socdem <- glm(form, data = ger_df_wide, family = binomial(link = "probit") ))

form <- as.formula(paste0("app_track_installed_at_w1 ~ ", paste0(covars_adapted_df$variable[covars_adapted_df$category == "Risk status and behavior"], collapse = "+")))
summary(probit_app_tracked_risk <- glm(form, data = ger_df_wide, family = binomial(link = "probit") ))

form <- as.formula(paste0("app_track_installed_at_w1 ~ ", paste0(covars_adapted_df$variable[covars_adapted_df$category == "Motivational factors"], collapse = "+")))
summary(probit_app_tracked_motiv <- glm(form, data = ger_df_wide, family = binomial(link = "probit") ))


# probit model of combined app installation (tracked + survey; pre-treatment) -------------
form <- as.formula(paste0("app_hyb_installed_at_w1 ~ ", paste0(covars_adapted_df$variable, collapse = "+")))
summary(probit_app_hybrid <- glm(form, data = ger_df_wide, family = binomial(link = "probit") ))

form <- as.formula(paste0("app_hyb_installed_at_w1 ~ ", paste0(covars_adapted_df$variable[covars_adapted_df$category == "Sociodemographics"], collapse = "+")))
summary(probit_app_hybrid_socdem <- glm(form, data = ger_df_wide, family = binomial(link = "probit") ))

form <- as.formula(paste0("app_hyb_installed_at_w1 ~ ", paste0(covars_adapted_df$variable[covars_adapted_df$category == "Risk status and behavior"], collapse = "+")))
summary(probit_app_hybrid_risk <- glm(form, data = ger_df_wide, family = binomial(link = "probit") ))

form <- as.formula(paste0("app_hyb_installed_at_w1 ~ ", paste0(covars_adapted_df$variable[covars_adapted_df$category == "Motivational factors"], collapse = "+")))
summary(probit_app_hybrid_motiv <- glm(form, data = ger_df_wide, family = binomial(link = "probit") ))



# coefficient plot, all models
pdf(file="../figures/probit-app-uptake-all.pdf", height=10, width=8, family="Helvetica")
theme_set(theme_bw())
plot_models(probit_app_tracked, probit_app_report, probit_app_hybrid, 
            axis.labels = rev(covars_adapted_df$var_label),
            axis.lim = c(-.5, 1),
            #std.est = "std2",
            show.p = TRUE,
            transform = NULL,
            m.labels = c("Tracked", "Reported", "Hybrid"),
            legend.title = "Outcome",
            axis.title = "Probit estimate",
            spacing = .4,
            colors = c("black","#e66101","#5e3c99"),
            grid = FALSE) + 
  geom_vline(xintercept = c(11.5, 19.5)) + 
  theme(plot.margin = unit(c(0.2,0.2,0,0.2), "cm"),
        legend.position="bottom")
dev.off()




