### -----------------------------
### descriptives
### -----------------------------

# load data --------------------------

ger_df_wide <- readRDS("../data/clean/ger_df_wide_an.RDS")
tracking_df_all <- readRDS("../data/clean/tracking_df_all_an.RDS")


# participation by sample/treatment/wave -----------------

table(ger_df_wide$sample_type.1)
table(ger_df_wide$sample_type.2)
table(ger_df_wide$sample_type.3)

table(ger_df_wide$treat.1)
table(ger_df_wide$treat.1, ger_df_wide$sample_type.1)
table(ger_df_wide$treat_incent.2)
table(ger_df_wide$treat_incent.2, ger_df_wide$sample_type.2)


# project costs calculation --------------
(2.3*1400  + # survey for survey-only, wave 1
2.1*1200 +  # survey for survey-only, wave 2
2.1*1050 +  # survey for survey-only, wave 3
3.0*650  +       # survey for survey-tracking, wave 1
3.0*590 +   # survey for survey-tracking, wave 2
3.0*530 +  # survey for survey-tracking, wave 2
2000 +  # incentivization costs
10800)*   # tracking data, 3 months
1.16 # VAT 

# best for planning calculation -------------------

fraction_smartphone_user <- .768 # fraction of reported smartphone users
baseline_pop <- 70.6 # Grundgesamtheit ist die deutschsprachige Wohnbevölkerung ab 14 Jahren in Deutschland, nach Hochrechnung der amtlichen Statistik zurzeit 70,60 Mio.
app_users <- 15 # estimated 100 days after launch; see https://www.n-tv.de/politik/Software-gut-Nutzer-nur-mittelmaessig-article22055409.html?utm_source=pocket-newtab-global-de-DE
(est_smartphone_users_tot <- baseline_pop*fraction_smartphone_user)
(app_users/est_smartphone_users_tot)




# panel timing -------------------------

# field times
(fieldtime_w1 <- range(ger_df_wide$StartDate.1, na.rm = TRUE))
(fieldtime_w2 <- range(ger_df_wide$StartDate.2, na.rm = TRUE))
(fieldtime_w3 <- range(ger_df_wide$StartDate.3, na.rm = TRUE))


# time between W1 and W2
ger_df_wide$w12gap_days <- ger_df_wide$StartDate.2 - ger_df_wide$EndDate.1
summary(as.numeric(ger_df_wide$w12gap_days))

# time between W2 and W3
ger_df_wide$w23gap_days <- ger_df_wide$StartDate.3 - ger_df_wide$EndDate.2
summary(as.numeric(ger_df_wide$w23gap_days))


# panel attrition -----------------------

# by wave
table(ger_df_wide$wave.1)
table(ger_df_wide$wave.2)
table(ger_df_wide$wave.3)

# by wave and sample type (manually added numbers to table in appendix)
table(ger_df_wide$sample_type.1, ger_df_wide$wave.1)
table(ger_df_wide$sample_type.1, ger_df_wide$wave.2)
table(ger_df_wide$sample_type.1, ger_df_wide$wave.3)




# models of selection into tracking panel / into survey among tracking panelists --------

# selection into tracking panel
ger_df_wide$tracking_panel <- ger_df_wide$sample_type.1 %in% c("surveytracking", "trackingonly" )
form <- as.formula(paste0("tracking_panel", " ~ ", paste0(c("age_cat.1", "female.1", "educ_cat.1", "data_control_1.1", "data_control_2.1", "data_control_3.1", "digliteracy_score.1"), collapse = "+")))
into_tracking <- lm(form, data = ger_df_wide, na.action = na.exclude)

# selection into survey among tracking panelists
ger_df_wide$tracking_panel_survey <- ger_df_wide$sample_type.1 %in% c("surveytracking")
form <- as.formula(paste0("tracking_panel_survey", " ~ ", paste0(c("age_cat.1", "female.1", "educ_cat.1"), collapse = "+")))
into_survey <- lm(form, data = filter(ger_df_wide, sample_type.1 != "surveyonly"), na.action = na.exclude)

# labels
varlabels <- c("Intercept", "Age: 30-39ys", "Age: 40-49ys", "Age: 50-59ys", "Age: 60+ys", "Gender: Female", "Education: Medium", "Education: High", "Control access to personal information", "Government protects my privacy", "Concerned about companies accessing data", "Data Literacy")

# export
texreg(list(into_tracking, into_survey), custom.coef.names = varlabels, custom.model.names = c("Tracking panel", "Tracking survey"), booktabs = TRUE, caption.above = TRUE, caption = "Model of selection into tracking panel and into survey among tracking panelists", float.pos = "t!h", label = "tab:selectionintotrackingsurvey", use.packages = FALSE, dcolumn = TRUE, file = "../figures/selectionintotrackingsurvey.tex")




# model panel attrition -------------------------

# predict attrition
ger_df_wide$w2_dropout <- is.na(ger_df_wide$wave.2)
ger_df_wide$w3_dropout <- !is.na(ger_df_wide$wave.2) & is.na(ger_df_wide$wave.3)

# incentivization agreement control value to false for everybody else
ger_df_wide$treat_incent_agree_control.2 <- ger_df_wide$treat_incent_agree.2
ger_df_wide$treat_incent_agree_control.2[is.na(ger_df_wide$treat_incent_agree.2) & !is.na(ger_df_wide$treat_incent_any.2)] <- FALSE

# ols models
out_w2 <- as.formula(paste0("w2_dropout", " ~ ", paste0(c("sample_type.1", "att_scale_score.1", "app_know_index.1", "app_hyb_installed_before_w1", "treat_any.1",  covars_adapted_df$variable[covars_adapted_df$category %in% c("Sociodemographics", "Motivational factors")]), collapse = "+"))) %>% glm(data = ger_df_wide, family = binomial(link = "probit"), na.action = na.exclude)

out_w3 <- as.formula(paste0("w3_dropout", " ~ ", paste0(c("sample_type.1", "att_scale_score.1", "app_know_index.1", "app_hyb_installed_before_w1", "treat_any.1", covars_adapted_df$variable[covars_adapted_df$category %in% c("Sociodemographics", "Motivational factors")]), collapse = "+"))) %>% glm(data = filter(ger_df_wide, !is.na(wave.2)), family = binomial(link = "probit"), na.action = na.exclude)

out_w3_incentonly <- as.formula(paste0("w3_dropout", " ~ ", paste0(c("sample_type.1", "att_scale_score.1", "app_know_index.1", "treat_any.1", "treat_incent_any.2", "treat_incent_agree_control.2", covars_adapted_df$variable[covars_adapted_df$category %in% c("Sociodemographics", "Motivational factors")]), collapse = "+"))) %>% glm(data = filter(ger_df_wide, !is.na(wave.2)), family = binomial(link = "probit"), na.action = na.exclude)
outcome_labels <- c("W2 dropout", "W3 dropout", "\\shortstack{W3 dropout, \\\\ app non-users}")

varlabels <- c("Intercept", "Tracking sample", "Attitudes towards app", "App knowledge", "App installed", "Message treatment W1", covars_adapted_df$var_label[covars_adapted_df$category %in% c("Sociodemographics", "Motivational factors")], "Incentivization treatment W2", "Incentivization agreement W2") %>%
  str_replace("\\n", " ") %>% str_replace("Household", "HH") %>% str_replace(" perception", "") %>% str_replace(" Family and Friends", " Family/Friends")

texreg(list(out_w2, out_w3, out_w3_incentonly), single.row = TRUE, custom.model.names = outcome_labels, custom.coef.names = varlabels, reorder.coef = c(1:6, 23:24, 7:22), booktabs = TRUE, caption.above = TRUE, caption = "Probit models of panel attrition. Outcome is coded 1 if respondent drops out before respective wave. The model resported in the last column is restricted to participants who report of not having installed the app in Wave 2 and therefore qualify for one of the treatment groups in the incentivization experiment.", float.pos = "h!", label = "tab:panelattrition-models", dcolumn = TRUE, use.packages = FALSE, file = "../figures/panelattrition-models.tex")


# model of tracked app installation age-digital literacy interaction (pre-treatment) -------------

form <- as.formula(paste0("app_track_installed_at_w1 ~ age_cat5.1"))
summary(app_track_age <- lm(form, data = ger_df_wide))

form <- as.formula(paste0("app_track_installed_at_w1 ~ age_cat5.1*digliteracy_score_cat2.1"))
summary(app_track_age_diglit <- lm(form, data = ger_df_wide))
form <- as.formula(paste0("app_rep_installed_at_w1 ~ age_cat5.1*digliteracy_score_cat2.1"))
summary(app_rep_age_diglit <- lm(form, data = ger_df_wide))

varlabels <- c("Intercept", 
               "Age: 30-39ys", "Age: 40-49ys", "Age: 50-59ys", "Age: 60+ys", 
               "Digital literacy: High", 
               "Age: 30-39ys X Digital literacy: High", "Age: 40-49ys X Digital literacy: High", "Age: 50-59ys X Digital literacy: High", "Age: 60+ys X Digital literacy: High") 

texreg(list(app_track_age_diglit), single.row = TRUE, custom.model.names = c("Tracked uptake"), custom.coef.names = varlabels, booktabs = TRUE, caption.above = TRUE, caption = "Linear model of tracked app usage as a function of age and digital literacy.", float.pos = "h!t", label = "tab:app-track-use-age-diglit-model", dcolumn = TRUE, use.packages = FALSE, file = "../figures/app-track-use-age-diglit-model.tex")

# additional crosstabs
with(ger_df_wide, prop.table(table(app_track_installed_at_w1, age_cat5.1), 2))
with(ger_df_wide, prop.table(table(app_track_installed_at_w1, digliteracy_score_cat2.1), 2))
with(ger_df_wide, prop.table(table(age_cat5.1, digliteracy_score_cat2.1), 1))



# tabulate reported vs. tracked usage -------------------------

# compare tracked with reported use (W1)
with(ger_df_wide, table(app_rep_installed_before_w1, app_track_installed_before_w1, useNA = "always"))
with(ger_df_wide, table(app_rep_installed_before_w2, app_track_installed_before_w2, useNA = "always"))
with(ger_df_wide, table(app_rep_installed_before_w3, app_track_installed_before_w3, useNA = "always"))

Reported <- ger_df_wide$app_rep_installed_before_w1
Tracked <- ger_df_wide$app_track_installed_before_w1

tab_out <- descr::CrossTable(Reported, Tracked,
                      prop.c = TRUE, prop.t = FALSE, prop.chisq = FALSE,
                      dnn = c("Reported", "Tracked"),
                      digits = 2)
tab_out
tab_out_xtab <- xtable(tab_out, label = "tab:appuse-reported-vs-tracked", caption = "Reported vs. tracked app usage. Tracking panel, before W1.")
print(tab_out_xtab, type = "latex", sanitize.text.function = function(x) {gsub('"',"",x)}, size = "small", table.placement = "h!",  booktabs = TRUE, include.rownames = TRUE, include.colnames = TRUE, caption.placement = "top", file = "../figures/appuse-reported-vs-tracked.tex")



# plot app usage rates (W1) across samples and measures ----

app_rep_surveyonly <- mean(ger_df_wide$app_rep_installed_before_w1[ger_df_wide$sample_type.1 == "surveyonly"], na.rm = TRUE)
app_rep_surveytrack <- mean(ger_df_wide$app_rep_installed_before_w1[ger_df_wide$sample_type.1 == "surveytracking"], na.rm = TRUE)
app_track_surveytrack <- mean(ger_df_wide$app_track_installed_before_w1[ger_df_wide$sample_type.1 == "surveytracking"], na.rm = TRUE)
app_track_trackonly <- mean(ger_df_wide$app_track_installed_before_w1[ger_df_wide$sample_type.1 == "trackingonly"], na.rm = TRUE)

dat <- data.frame(sample = c("Survey-only\n(reported)", "Survey-Tracking\n(reported)", "Survey-Tracking\n(tracked)", "Tracking-only \n(tracked)"),
           app_usage = c(app_rep_surveyonly, app_rep_surveytrack, app_track_surveytrack, app_track_trackonly))

pdf(file="../figures/app-usage-by-sample-and-measure.pdf", height = 4, width = 7)
ggplot(dat, aes(sample, app_usage)) +
  geom_col() + 
  annotate("text", x = 1:4, y = dat$app_usage + .02, label = round(dat$app_usage, 2), size = 4) + 
  xlab("") + 
  ylab("Proportion")
dev.off()


# plot reported Bluetooth usage behavior ----

dat <- as.data.frame(table(ger_df$app_bluetooth_act, ger_df$wave))
names(dat) <- c("category", "Wave", "freq")
dat$label <- factor(dplyr::recode(dat$category, `1` = "Never", `2` = "Rarely", `3` = "Sometimes", `4` = "Mostly", `5` = "Always"), levels = c("Never", "Rarely", "Sometimes", "Mostly", "Always"))
dat %<>%  group_by(Wave) %>%  mutate(n_tot = sum(freq))
dat$prop <- dat$freq / dat$n_tot

pdf(file="../figures/bluetooth-usage-selfreport.pdf", height = 4, width = 7)
ggplot(dat, aes(fill = Wave, y = prop, x = label, label = round(prop, 2))) + 
  geom_bar(position = "dodge", stat = "identity") + 
  geom_text(position = position_dodge2(width = .9, preserve = "single"), hjust=.5, vjust=-.5) + 
  scale_fill_manual(values = c("#74a9cf", "#2b8cbe", "#045a8d")) +
  ylab("Proportion") + xlab("") + 
  ylim(0, .85) + 
  theme(legend.position="bottom")
dev.off()


# plot report positive test result to app ----

dat <- as.data.frame(table(ger_df$app_submit_positive_result, ger_df$wave))
names(dat) <- c("category", "Wave", "freq")
dat$label <- dplyr::recode(dat$category, `1` = "1 = Certainly\nnot report", `2` = "2", `3` = "3", `4` = "4", `5`= "5", `6` = "6", `7` = "7 = Certainly\nreport")
dat %<>%  group_by(Wave) %>%  mutate(n_tot = sum(freq))
dat$prop <- dat$freq / dat$n_tot

pdf(file="../figures/report-positive-test-to-app.pdf", height = 4, width = 9)
ggplot(dat, aes(fill = Wave, y = prop, x = label, label = round(prop, 2))) + 
  geom_bar(position = "dodge", stat = "identity") + 
  geom_text(position = position_dodge2(width = .9, preserve = "single"), hjust=.5, vjust=-.5) + 
  scale_fill_manual(values = c("#74a9cf", "#2b8cbe", "#045a8d")) +
  ylab("Proportion") + xlab("") + 
  ylim(0, .9) + 
  theme(legend.position="bottom")
dev.off()


# plot getting tested/quarantined after app risk warning  ----

dat <- as.data.frame(table(ger_df$app_test_warning, ger_df$wave))
names(dat) <- c("category", "Wave", "freq")
dat$label <- dplyr::recode(dat$category, `1` = "1 = Certainly not getting\ntested/quarantined", `2` = "2", `3` = "3", `4` = "4", `5`= "5", `6` = "6", `7` = "7 = Certainly getting\ntested/quarantined")
dat %<>%  group_by(Wave) %>%  mutate(n_tot = sum(freq))
dat$prop <- dat$freq / dat$n_tot

pdf(file="../figures/getting-tested-after-warning.pdf", height = 4, width = 9)
ggplot(dat, aes(fill = Wave, y = prop, x = label, label = round(prop, 2))) + 
  geom_bar(position = "dodge", stat = "identity") + 
  geom_text(position = position_dodge2(width = .9, preserve = "single"), hjust=.5, vjust=-.5) + 
  scale_fill_manual(values = c("#74a9cf", "#2b8cbe", "#045a8d")) +
  ylab("Proportion") + xlab("") + 
  ylim(0, .8) + 
  theme(legend.position="bottom")
dev.off()



# reasons not to use app --------------------

n_base <- sum(!is.na(ger_w3_time_df$t_app_why_no_install_Page_Submit))
dat <- ger_df_wide %>% dplyr::select(contains("app_why_no_install") & ends_with(".3") & !contains("TEXT"))
reasons_labels <- sapply(dat, label) %>% str_replace(fixed("Können Sie uns sagen, weshalb Sie die App nicht installiert haben oder wieder deinstalliert haben?\n(Mehrere Antworten möglich) - Selected Choice "), "")
reasons_labels_translated <- c(
"I don't think that the app is of any use.",                         
"I am concerned about privacy.",     
"I don't want the state to monitor me.",                    
"Bluetooth on my smartphone has to be activated permanently.",
"I don't want the app to track who I meet.",              
"I think the Corona topic is exaggerated",                       
"I want to wait for the experiences of others first." ,                
"My smartphone is too old.",            
"The app uses too much battery." ,      
"The app produces error messages." ,  
"I don't feel well enough informed." ,                         
"I have not yet installed the app, but I plan to." ,       
"Other reasons, namely:")

sum_selects <- sapply(dat, sum, na.rm = TRUE)
frac_selects <- sum_selects/n_base
no_install_df <- data.frame(
           variable = names(dat),
           var_label_ger = reasons_labels,
           var_label_eng = reasons_labels_translated,
           value = frac_selects,
           stringsAsFactors = FALSE)
no_install_df <- filter(no_install_df, variable != "app_why_no_install_13.3") %>%
  arrange(value)
no_install_df$var_label_eng <- factor(no_install_df$var_label_eng, levels = no_install_df$var_label_eng)

pdf(file="../figures/app-reasons-why-no-install.pdf", height = 5, width = 7)
ggplot(no_install_df, aes(var_label_eng, value)) +
  geom_bar(stat = "identity") + 
  geom_text(aes(label = round(value, 2), y = value + .03), size = 3) + 
  xlab("") + 
  ylab("Proportion of mentions") + 
  coord_flip()
dev.off()




# tracked app usage across covariates before and after treatments, survey-tracking sample (figure for main text) -------------------------

### compute tracked app uptake rates by group, at W1
sum_list <- list()
covars_cat <- covars_df$variable
for (i in seq_along(covars_cat)){
  sum_out <- filter(ger_df_wide, sample_type.1 == "surveytracking") %>% 
    group_by(.data[[covars_cat[i]]]) %>% 
    summarize(prop = mean(app_track_installed_before_w1, na.rm = TRUE),
              prop_cilo = prop - 1.39*sqrt((prop*(1-prop))/n()),
              prop_cihi = prop + 1.39*sqrt((prop*(1-prop))/n())
              ) 
  sum_out$var <- covars_cat[i]
  names(sum_out) <- c("val_label", "prop", "prop_cilo", "prop_cihi", "varname")
  sum_out <- dplyr::select(sum_out, varname, val_label, prop, prop_cilo, prop_cihi)
  sum_list[[i]] <- sum_out
}

# combine results
app_usage_means_df <- bind_rows(sum_list)
app_usage_means_df <- filter(app_usage_means_df, !is.na(val_label))
app_usage_means_df <- left_join(app_usage_means_df, covars_df, by = c("varname" = "variable"))

# prepare data for plot
app_uptake_tracked <- app_usage_means_df %>% filter(!(val_label == "Medium" & var_label != "Education"))
app_uptake_tracked$val_label <- as.character(app_uptake_tracked$val_label) %>% str_replace_all("\\n", " ")
app_uptake_tracked$y_pos <- rev(seq_len(nrow(app_uptake_tracked)))
app_uptake_tracked <- app_uptake_tracked %>% group_by(varname) %>% mutate(cat_count = n(),  y_pos_cat = mean(y_pos), max_y_pos = max(y_pos)) %>% ungroup() %>% mutate(first_cat = y_pos == max_y_pos)
app_uptake_tracked$prop_mean <- filter(ger_df_wide, sample_type.1 == "surveytracking") %>% summarize(prop_mean = mean(app_track_installed_before_w1, na.rm = TRUE)) %>% as.numeric()

# set colors
n_cols <- length(unique(app_uptake_tracked$var_label))
cols <- c('#b2182b','#2166ac')
cols_sorted <- rep(cols, length.out = n_cols)
col_df <- data.frame(var_label = unique(app_uptake_tracked$var_label), col = cols_sorted)
app_uptake_tracked <- merge(app_uptake_tracked, col_df, by = "var_label")


### compute tracked app uptake rates by group, at W3
sum_list <- list()
covars_cat <- covars_df$variable
for (i in seq_along(covars_cat)){
  sum_out <- filter(ger_df_wide, sample_type.1 == "surveytracking") %>% 
    group_by(.data[[covars_cat[i]]]) %>% 
    summarize(prop = mean(app_track_installed_at_w3, na.rm = TRUE),
              prop_cilo = prop - 1.39*sqrt((prop*(1-prop))/n()),
              prop_cihi = prop + 1.39*sqrt((prop*(1-prop))/n())
    ) 
  sum_out$var <- covars_cat[i]
  names(sum_out) <- c("val_label", "prop", "prop_cilo", "prop_cihi", "varname")
  sum_out <- dplyr::select(sum_out, varname, val_label, prop, prop_cilo, prop_cihi)
  sum_list[[i]] <- sum_out
}

# combine results
app_usage_w3_means_df <- bind_rows(sum_list)
app_usage_w3_means_df <- filter(app_usage_w3_means_df, !is.na(val_label))
app_usage_w3_means_df <- left_join(app_usage_w3_means_df, covars_df, by = c("varname" = "variable"))

# prepare data for plot
app_uptake_w3_tracked <- app_usage_w3_means_df %>% filter(!(val_label == "Medium" & var_label != "Education"))
app_uptake_w3_tracked$val_label <- as.character(app_uptake_w3_tracked$val_label) %>% str_replace_all("\\n", " ")
app_uptake_w3_tracked$y_pos <- rev(seq_len(nrow(app_uptake_w3_tracked)))
app_uptake_w3_tracked <- app_uptake_w3_tracked %>% group_by(varname) %>% mutate(cat_count = n(),  y_pos_cat = mean(y_pos), max_y_pos = max(y_pos)) %>% ungroup() %>% mutate(first_cat = y_pos == max_y_pos)
app_uptake_w3_tracked$prop_mean <- filter(ger_df_wide, sample_type.1 == "surveytracking") %>% summarize(prop_mean = mean(app_track_installed_at_w3, na.rm = TRUE)) %>% as.numeric()

# set colors
n_cols <- length(unique(app_uptake_w3_tracked$var_label))
cols <- c('#b2182b','#2166ac')
cols_sorted <- rep(cols, length.out = n_cols)
col_df <- data.frame(var_label = unique(app_uptake_w3_tracked$var_label), col = cols_sorted)
app_uptake_w3_tracked <- merge(app_uptake_w3_tracked, col_df, by = "var_label")


### compute reported app uptake rates by group, at W1
sum_list <- list()
covars_cat <- covars_df$variable
for (i in seq_along(covars_cat)){
  sum_out <- filter(ger_df_wide, sample_type.1 == "surveytracking") %>% 
    group_by(.data[[covars_cat[i]]]) %>% 
    summarize(prop = mean(app_rep_installed_before_w1, na.rm = TRUE),
              prop_cilo = prop - 1.39*sqrt((prop*(1-prop))/n()),
              prop_cihi = prop + 1.39*sqrt((prop*(1-prop))/n())
    ) 
  sum_out$var <- covars_cat[i]
  names(sum_out) <- c("val_label", "prop", "prop_cilo", "prop_cihi", "varname")
  sum_out <- dplyr::select(sum_out, varname, val_label, prop, prop_cilo, prop_cihi)
  sum_list[[i]] <- sum_out
}

# combine results
app_usage_means_df <- bind_rows(sum_list)
app_usage_means_df <- filter(app_usage_means_df, !is.na(val_label))
app_usage_means_df <- left_join(app_usage_means_df, covars_df, by = c("varname" = "variable"))

# prepare data for plot
app_uptake_reported <- app_usage_means_df %>% filter(!(val_label == "Medium" & var_label != "Education"))
app_uptake_reported$val_label <- as.character(app_uptake_reported$val_label) %>% str_replace_all("\\n", " ")
app_uptake_reported$y_pos <- rev(seq_len(nrow(app_uptake_reported)))
app_uptake_reported <- app_uptake_reported %>% group_by(varname) %>% mutate(cat_count = n(),  y_pos_cat = mean(y_pos), max_y_pos = max(y_pos)) %>% ungroup() %>% mutate(first_cat = y_pos == max_y_pos)
app_uptake_reported$prop_mean <- filter(ger_df_wide, sample_type.1 == "surveytracking") %>% summarize(prop_mean = mean(app_rep_installed_before_w1, na.rm = TRUE)) %>% as.numeric()

# set colors
n_cols <- length(unique(app_uptake_reported$var_label))
cols <- c('#b2182b','#2166ac')
cols_sorted <- rep(cols, length.out = n_cols)
col_df <- data.frame(var_label = unique(app_uptake_reported$var_label), col = cols_sorted)
app_uptake_reported <- merge(app_uptake_reported, col_df, by = "var_label")


### compute reported app uptake rates by group, at W3
sum_list <- list()
covars_cat <- covars_df$variable
for (i in seq_along(covars_cat)){
  sum_out <- filter(ger_df_wide, sample_type.1 == "surveytracking") %>% 
    group_by(.data[[covars_cat[i]]]) %>% 
    summarize(prop = mean(app_rep_installed_at_w3, na.rm = TRUE),
              prop_cilo = prop - 1.39*sqrt((prop*(1-prop))/n()),
              prop_cihi = prop + 1.39*sqrt((prop*(1-prop))/n())
    ) 
  sum_out$var <- covars_cat[i]
  names(sum_out) <- c("val_label", "prop", "prop_cilo", "prop_cihi", "varname")
  sum_out <- dplyr::select(sum_out, varname, val_label, prop, prop_cilo, prop_cihi)
  sum_list[[i]] <- sum_out
}

# combine results
app_usage_w3_means_df <- bind_rows(sum_list)
app_usage_w3_means_df <- filter(app_usage_w3_means_df, !is.na(val_label))
app_usage_w3_means_df <- left_join(app_usage_w3_means_df, covars_df, by = c("varname" = "variable"))

# prepare data for plot
app_uptake_w3_reported <- app_usage_w3_means_df %>% filter(!(val_label == "Medium" & var_label != "Education"))
app_uptake_w3_reported$val_label <- as.character(app_uptake_w3_reported$val_label) %>% str_replace_all("\\n", " ")
app_uptake_w3_reported$y_pos <- rev(seq_len(nrow(app_uptake_w3_reported)))
app_uptake_w3_reported <- app_uptake_w3_reported %>% group_by(varname) %>% mutate(cat_count = n(),  y_pos_cat = mean(y_pos), max_y_pos = max(y_pos)) %>% ungroup() %>% mutate(first_cat = y_pos == max_y_pos)
app_uptake_w3_reported$prop_mean <- filter(ger_df_wide, sample_type.1 == "surveytracking") %>% summarize(prop_mean = mean(app_rep_installed_at_w3, na.rm = TRUE)) %>% as.numeric()

# set colors
n_cols <- length(unique(app_uptake_w3_reported$var_label))
cols <- c('#b2182b','#2166ac')
cols_sorted <- rep(cols, length.out = n_cols)
col_df <- data.frame(var_label = unique(app_uptake_w3_reported$var_label), col = cols_sorted)
app_uptake_w3_reported <- merge(app_uptake_w3_reported, col_df, by = "var_label")



### compute mean daily app usage 1w prior to W1, by group
sum_list <- list()
mindate <- as.Date(min(ger_w1_df$StartDate)) - 7 # only obs that had installed the app at least 7 days prior to W3
covars_cat <- covars_df$variable
for (i in seq_along(covars_cat)){
  sum_out <- filter(ger_df_wide, sample_type.1 == "surveytracking" & (as.Date(track_date_first_install.1) <= mindate)) %>% 
    group_by(.data[[covars_cat[i]]]) %>% 
    summarize(dailyavg = mean(track_usage_daily_avg_7days_before_w1.1, na.rm = TRUE),
              dailyavg_cilo = dailyavg - 1.39*(sd(track_usage_daily_avg_7days_before_w1.1, na.rm = TRUE)/sqrt(n())),
              dailyavg_cihi = dailyavg + 1.39*(sd(track_usage_daily_avg_7days_before_w1.1, na.rm = TRUE)/sqrt(n()))
    ) 
  sum_out$var <- covars_cat[i]
  names(sum_out) <- c("val_label", "dailyavg", "dailyavg_cilo", "dailyavg_cihi", "varname")
  sum_out <- dplyr::select(sum_out, varname, val_label, dailyavg, dailyavg_cilo, dailyavg_cihi)
  sum_list[[i]] <- sum_out
}

# combine results
daily_usage_means_df <- bind_rows(sum_list)
daily_usage_means_df <- filter(daily_usage_means_df, !is.na(val_label))
daily_usage_means_df <- left_join(daily_usage_means_df, covars_df, by = c("varname" = "variable"))

# prepare data for plot
daily_usage_tracked <- daily_usage_means_df %>% filter(!(val_label == "Medium" & var_label != "Education"))
daily_usage_tracked$val_label <- as.character(daily_usage_tracked$val_label) %>% str_replace_all("\\n", " ")
daily_usage_tracked$y_pos <- rev(seq_len(nrow(daily_usage_tracked)))
daily_usage_tracked <- daily_usage_tracked %>% group_by(varname) %>% mutate(cat_count = n(),  y_pos_cat = mean(y_pos), max_y_pos = max(y_pos)) %>% ungroup() %>% mutate(first_cat = y_pos == max_y_pos)
daily_usage_tracked$dailyavg_mean <- filter(ger_df_wide, sample_type.1 == "surveytracking" & (as.Date(track_date_first_install.1) <= mindate)) %>% summarize(dailyavg_mean = mean(track_usage_daily_avg_7days_before_w1.1, na.rm = TRUE)) %>% as.numeric()

# set colors
n_cols <- length(unique(daily_usage_tracked$var_label))
cols <- c('#b2182b','#2166ac')
cols_sorted <- rep(cols, length.out = n_cols)
col_df <- data.frame(var_label = unique(daily_usage_tracked$var_label), col = cols_sorted)
daily_usage_tracked <- merge(daily_usage_tracked, col_df, by = "var_label")


### compute mean daily app usage 1w prior to W3, by group
sum_list <- list()
mindate <- as.Date(min(ger_w3_df$StartDate)) - 7 # only obs that had installed the app at least 7 days prior to W3
covars_cat <- covars_df$variable
for (i in seq_along(covars_cat)){
  sum_out <- filter(ger_df_wide, sample_type.1 == "surveytracking" & (as.Date(track_date_first_install.1) <= mindate)) %>% 
    group_by(.data[[covars_cat[i]]]) %>% 
    summarize(dailyavg = mean(track_usage_daily_avg_7days_before_w3.1, na.rm = TRUE),
              dailyavg_cilo = dailyavg - 1.39*(sd(track_usage_daily_avg_7days_before_w3.1, na.rm = TRUE)/sqrt(n())),
              dailyavg_cihi = dailyavg + 1.39*(sd(track_usage_daily_avg_7days_before_w3.1, na.rm = TRUE)/sqrt(n()))
    ) 
  sum_out$var <- covars_cat[i]
  names(sum_out) <- c("val_label", "dailyavg", "dailyavg_cilo", "dailyavg_cihi", "varname")
  sum_out <- dplyr::select(sum_out, varname, val_label, dailyavg, dailyavg_cilo, dailyavg_cihi)
  sum_list[[i]] <- sum_out
}

# combine results
daily_usage_w3_means_df <- bind_rows(sum_list)
daily_usage_w3_means_df <- filter(daily_usage_w3_means_df, !is.na(val_label))
daily_usage_w3_means_df <- left_join(daily_usage_w3_means_df, covars_df, by = c("varname" = "variable"))

# prepare data for plot
daily_usage_w3_tracked <- daily_usage_w3_means_df %>% filter(!(val_label == "Medium" & var_label != "Education"))
daily_usage_w3_tracked$val_label <- as.character(daily_usage_w3_tracked$val_label) %>% str_replace_all("\\n", " ")
daily_usage_w3_tracked$y_pos <- rev(seq_len(nrow(daily_usage_w3_tracked)))
daily_usage_w3_tracked <- daily_usage_w3_tracked %>% group_by(varname) %>% mutate(cat_count = n(),  y_pos_cat = mean(y_pos), max_y_pos = max(y_pos)) %>% ungroup() %>% mutate(first_cat = y_pos == max_y_pos)
daily_usage_w3_tracked$dailyavg_mean <- filter(ger_df_wide, sample_type.1 == "surveytracking" & (as.Date(track_date_first_install.1) <= mindate)) %>% summarize(dailyavg_mean = mean(track_usage_daily_avg_7days_before_w3.1, na.rm = TRUE)) %>% as.numeric()

# set colors
n_cols <- length(unique(daily_usage_w3_tracked$var_label))
cols <- c('#b2182b','#2166ac')
cols_sorted <- rep(cols, length.out = n_cols)
col_df <- data.frame(var_label = unique(daily_usage_w3_tracked$var_label), col = cols_sorted)
daily_usage_w3_tracked <- merge(daily_usage_w3_tracked, col_df, by = "var_label")



### compute bluetooth usage rates by group, at W1
sum_bluetooth_list <- list()
covars_cat <- covars_df$variable
for (i in seq_along(covars_cat)){
  sum_out <- filter(ger_df_wide, sample_type.1 == "surveytracking" & app_track_installed_before_w1 == TRUE) %>% 
    group_by(.data[[covars_cat[i]]]) %>% 
    summarize(prop = mean(app_bluetooth.1 == 1, na.rm = TRUE),
              prop_cilo = prop - 1.39*sqrt((prop*(1-prop))/n()),
              prop_cihi = prop + 1.39*sqrt((prop*(1-prop))/n()),
    ) 
  sum_out$var <- covars_cat[i]
  names(sum_out) <- c("val_label", "prop", "prop_cilo", "prop_cihi", "varname")
  sum_out <- dplyr::select(sum_out, varname, val_label, prop, prop_cilo, prop_cihi)
  sum_bluetooth_list[[i]] <- sum_out
}

# combine results
bt_usage_means_df <- bind_rows(sum_bluetooth_list)
bt_usage_means_df <- filter(bt_usage_means_df, !is.na(val_label))
bt_usage_means_df <- left_join(bt_usage_means_df, covars_df, by = c("varname" = "variable"))

# prepare data for plot
bt_usage_tracked <- bt_usage_means_df %>% filter(!(val_label == "Medium" & var_label != "Education"))
bt_usage_tracked$val_label <- as.character(bt_usage_tracked$val_label) %>% str_replace_all("\\n", " ")
bt_usage_tracked$y_pos <- rev(seq_len(nrow(bt_usage_tracked)))
bt_usage_tracked <- bt_usage_tracked %>% group_by(varname) %>% mutate(cat_count = n(),  y_pos_cat = mean(y_pos), max_y_pos = max(y_pos)) %>% ungroup() %>% mutate(first_cat = y_pos == max_y_pos)
bt_usage_tracked$prop_mean <- filter(ger_df_wide, sample_type.1 == "surveytracking" & app_track_installed_before_w1 == TRUE) %>% summarize(prop_mean = mean(app_bluetooth.1 == 1, na.rm = TRUE)) %>% as.numeric()

# set colors
n_cols <- length(unique(bt_usage_tracked$var_label))
cols <- c('#b2182b','#2166ac')
cols_sorted <- rep(cols, length.out = n_cols)
col_df <- data.frame(var_label = unique(bt_usage_tracked$var_label), col = cols_sorted)
bt_usage_tracked <- merge(bt_usage_tracked, col_df, by = "var_label")


### compute bluetooth usage rates by group, at W3
sum_bluetooth_list <- list()
covars_cat <- covars_df$variable
for (i in seq_along(covars_cat)){
  sum_out <- filter(ger_df_wide, sample_type.1 == "surveytracking" & app_track_installed_before_w1 == TRUE) %>% 
    group_by(.data[[covars_cat[i]]]) %>% 
    summarize(prop = mean(app_bluetooth.3 == 1, na.rm = TRUE),
              prop_cilo = prop - 1.39*sqrt((prop*(1-prop))/n()),
              prop_cihi = prop + 1.39*sqrt((prop*(1-prop))/n()),
    ) 
  sum_out$var <- covars_cat[i]
  names(sum_out) <- c("val_label", "prop", "prop_cilo", "prop_cihi", "varname")
  sum_out <- dplyr::select(sum_out, varname, val_label, prop, prop_cilo, prop_cihi)
  sum_bluetooth_list[[i]] <- sum_out
}

# combine results
bt_usage_w3_means_df <- bind_rows(sum_bluetooth_list)
bt_usage_w3_means_df <- filter(bt_usage_w3_means_df, !is.na(val_label))
bt_usage_w3_means_df <- left_join(bt_usage_w3_means_df, covars_df, by = c("varname" = "variable"))

# prepare data for plot
bt_usage_w3_tracked <- bt_usage_w3_means_df %>% filter(!(val_label == "Medium" & var_label != "Education"))
bt_usage_w3_tracked$val_label <- as.character(bt_usage_w3_tracked$val_label) %>% str_replace_all("\\n", " ")
bt_usage_w3_tracked$y_pos <- rev(seq_len(nrow(bt_usage_w3_tracked)))
bt_usage_w3_tracked <- bt_usage_w3_tracked %>% group_by(varname) %>% mutate(cat_count = n(),  y_pos_cat = mean(y_pos), max_y_pos = max(y_pos)) %>% ungroup() %>% mutate(first_cat = y_pos == max_y_pos)
bt_usage_w3_tracked$prop_mean <- filter(ger_df_wide, sample_type.1 == "surveytracking" & app_track_installed_before_w1 == TRUE) %>% summarize(prop_mean = mean(app_bluetooth.3 == 1, na.rm = TRUE)) %>% as.numeric()

# set colors
n_cols <- length(unique(bt_usage_w3_tracked$var_label))
cols <- c('#b2182b','#2166ac')
cols_sorted <- rep(cols, length.out = n_cols)
col_df <- data.frame(var_label = unique(bt_usage_w3_tracked$var_label), col = cols_sorted)
bt_usage_w3_tracked <- merge(bt_usage_w3_tracked, col_df, by = "var_label")



### plot tracked app uptake by subgroups (figure for main text)
pdf(file="../figures/app-tracked-covariates.pdf", height = 12, width = 7, family="Helvetica")
par(oma=c(0,0,0,0))
par(mar=c(3,14,2.5,1))
# plot app uptake
plot(app_uptake_tracked$prop, 
     app_uptake_tracked$y_pos,
     xlim = c(0, .7),
     ylim = c(0.5, max(app_uptake_tracked$y_pos) + .5),
     yaxs="i", 
     yaxt = "n", 
     xaxt = "n",
     ylab = "",
     xlab = "")
# grid
#abline(h = app_uptake_tracked$y_pos, col = "grey")
abline(h = app_uptake_tracked$max_y_pos + .5, col = "black")
abline(v = seq(0, .7, .1), col = "grey")
abline(v = app_uptake_tracked$prop_mean, col = "black")
# confidence intervals
segments(x0 = app_uptake_tracked$prop_cilo, x1 = app_uptake_tracked$prop_cihi, y0 = app_uptake_tracked$y_pos, col = app_uptake_tracked$col)
# points
points(app_uptake_tracked$prop, 
       app_uptake_tracked$y_pos, pch = 19, col = app_uptake_tracked$col, bg = app_uptake_tracked$col)
# axes
axis(1, at = seq(0, .7, .1), label = seq(0, .7, .1))
Map(axis, side = 2, at = app_uptake_tracked$y_pos, col.axis = app_uptake_tracked$col, labels = app_uptake_tracked$val_label, las=1)
#axis(2,at=1:3,labels=FALSE)
#axis(2, at = app_uptake_tracked$y_pos, app_uptake_tracked$val_label, las = 1, col.axis = app_uptake_tracked$col)
Map(axis, side = 2, at = unique(app_uptake_tracked$y_pos_cat), col.axis = app_uptake_tracked$col[app_uptake_tracked$first_cat == TRUE], labels = unique(str_replace(app_uptake_tracked$var_label, "Risk behavior:\\n", "")), line = 10, tick = FALSE, las = 1, hadj = 0)
par(xpd=TRUE)
rect(ybottom = max(app_uptake_tracked$y_pos) + .5, ytop = max(app_uptake_tracked$y_pos) + 1.5, xleft = -.028, xright = .728, col = "lightgrey", border = "black")
par(xpd=FALSE)
axis(3, at = 0.35, label = "App uptake rate (tracked)", line = -.9, tick = FALSE)
# variable group labels
axis(2, at = mean(app_uptake_tracked$y_pos[app_uptake_tracked$category == "Sociodemographics"]), label = "Sociodemographics", line = 11, tick = FALSE)
axis(2, at = mean(app_uptake_tracked$y_pos[app_uptake_tracked$category == "Risk status and behavior"]), label = "Risk status and behavior", line = 11, tick = FALSE)
axis(2, at = mean(app_uptake_tracked$y_pos[app_uptake_tracked$category == "Motivational factors"]), label = "Attitudes", line = 11, tick = FALSE)
par(xpd=TRUE)
xleft <- -.47
segments(x0 = xleft, y0 = min(app_uptake_tracked$y_pos[app_uptake_tracked$category == "Sociodemographics"]), y1 = max(app_uptake_tracked$y_pos[app_uptake_tracked$category == "Sociodemographics"]))
segments(x0 = xleft, y0 = min(app_uptake_tracked$y_pos[app_uptake_tracked$category == "Risk status and behavior"]), y1 = max(app_uptake_tracked$y_pos[app_uptake_tracked$category == "Risk status and behavior"]))
segments(x0 = xleft, y0 = min(app_uptake_tracked$y_pos[app_uptake_tracked$category == "Motivational factors"]), y1 = max(app_uptake_tracked$y_pos[app_uptake_tracked$category == "Motivational factors"]))
par(xpd=FALSE)
dev.off()




app_uptake_tracked$var_label_ger <- 
  c("Alter", "Alter", "Alter", "Alter", "Alter",
    "Kinder", "Kinder",
    "COVID-Besorgnis:\nFreunde", "COVID-Besorgnis:\nFreunde",
    "COVID-Besorgnis:\nSelbst", "COVID-Besorgnis:\nFreunde",
    "COVID-Fall\nin Netzwerk", "COVID-Fall\nin Netzwerk",
    "Digitalkompetenz", "Digitalkompetenz",
    "Bildung", "Bildung", "Bildung",
    "Geschlecht", "Geschlecht",
    "Haushaltseinkommen", "Haushaltseinkommen", "Haushaltseinkommen",
    "Lebt in\nurbaner Region", "Lebt in\nurbaner Region",
    "Lebt in\nHochinzidenzregion", "Lebt in\nHochinzidenzregion",
    "Bedürfnis nach\nDatenkontrolle", "Bedürfnis nach\nDatenkontrolle",
    "AHA-Compliance", "AHA-Compliance",
    "Vorerkrankung", "Vorerkrankung",
    "Besuch von\nFreunden/Familie", "Besuch von\nFreunden/Familie",
    "Nutzung öffentl.\nVerkehrsmittel", "Nutzung öffentl.\nVerkehrsmittel", 
    "Besuch von\nRestaurant/Café/Bar", "Besuch von\nRestaurant/Café/Bar",
    "Eigeninteresse", "Eigeninteresse",
    "Soziale Verantwortung", "Soziale Verantwortung",
    "Vertrauen in\nRegierung", "Vertrauen in\nRegierung",
    "Vertrauen in\nGesundheitssystem", "Vertrauen in\nGesundheitssystem",
    "Vertrauen in\nWissenschaft", "Vertrauen in\nWissenschaft",
    "Arbeitend", "Arbeitend")
app_uptake_tracked$val_label_ger <- 
  c("18-29", "30-39", "40-49", "50-59", "60+",
    "Nein", "Ja", "Niedrig", "Hoch", "Niedrig", "Hoch", "Nein", "Ja", "Niedrig", "Hoch", "Niedrig", "Mittel", "Hoch", "Mann", "Frau", "<1.5T", "1.5-3T", ">3T", "Nein", "Ja", "Nein", "Ja", "Niedrig", "Hoch", "Nein", "Ja", "Nein", "Ja", "Niedrig", "Hoch", "Niedrig", "Hoch",  "Niedrig", "Hoch",  "Niedrig", "Hoch",  "Niedrig", "Hoch",  "Niedrig", "Hoch",  "Niedrig", "Hoch",  "Niedrig", "Hoch", "Nein", "Ja")
    

  

### plot tracked app uptake by subgroups (figure for main text)
pdf(file="../figures/app-tracked-covariates-ger.pdf", height = 12, width = 8, family="Helvetica")
par(oma=c(0,0,0,0))
par(mar=c(3,16,2.5,1))
# plot app uptake
plot(app_uptake_tracked$prop, 
     app_uptake_tracked$y_pos,
     xlim = c(0, .7),
     ylim = c(0.5, max(app_uptake_tracked$y_pos) + .5),
     yaxs="i", 
     yaxt = "n", 
     xaxt = "n",
     ylab = "",
     xlab = "")
# grid
#abline(h = app_uptake_tracked$y_pos, col = "grey")
abline(h = app_uptake_tracked$max_y_pos + .5, col = "black")
abline(v = seq(0, .7, .1), col = "grey")
abline(v = app_uptake_tracked$prop_mean, col = "black")
# confidence intervals
segments(x0 = app_uptake_tracked$prop_cilo, x1 = app_uptake_tracked$prop_cihi, y0 = app_uptake_tracked$y_pos, col = app_uptake_tracked$col)
# points
points(app_uptake_tracked$prop, 
       app_uptake_tracked$y_pos, pch = 19, col = app_uptake_tracked$col, bg = app_uptake_tracked$col)
# axes
axis(1, at = seq(0, .7, .1), label = seq(0, .7, .1))
Map(axis, side = 2, at = app_uptake_tracked$y_pos, col.axis = app_uptake_tracked$col, labels = app_uptake_tracked$val_label_ger, las=1)
Map(axis, side = 2, at = unique(app_uptake_tracked$y_pos_cat), col.axis = app_uptake_tracked$col[app_uptake_tracked$first_cat == TRUE], labels = unique(str_replace(app_uptake_tracked$var_label_ger, "Risk behavior:\\n", "")), line = 12, tick = FALSE, las = 1, hadj = 0)
par(xpd=TRUE)
rect(ybottom = max(app_uptake_tracked$y_pos) + .5, ytop = max(app_uptake_tracked$y_pos) + 1.5, xleft = -.028, xright = .728, col = "lightgrey", border = "black")
par(xpd=FALSE)
axis(3, at = 0.35, label = "Installationsrate (Tracking)", line = -.9, tick = FALSE)
# variable group labels
axis(2, at = mean(app_uptake_tracked$y_pos[app_uptake_tracked$category == "Sociodemographics"]), label = "Soziodemographie", line = 13, tick = FALSE)
axis(2, at = mean(app_uptake_tracked$y_pos[app_uptake_tracked$category == "Risk status and behavior"]), label = "Risikostatus und -verhalten", line = 13, tick = FALSE)
axis(2, at = mean(app_uptake_tracked$y_pos[app_uptake_tracked$category == "Motivational factors"]), label = "Einstellungen", line = 13, tick = FALSE)
par(xpd=TRUE)
xleft <- -.47
segments(x0 = xleft, y0 = min(app_uptake_tracked$y_pos[app_uptake_tracked$category == "Sociodemographics"]), y1 = max(app_uptake_tracked$y_pos[app_uptake_tracked$category == "Sociodemographics"]))
segments(x0 = xleft, y0 = min(app_uptake_tracked$y_pos[app_uptake_tracked$category == "Risk status and behavior"]), y1 = max(app_uptake_tracked$y_pos[app_uptake_tracked$category == "Risk status and behavior"]))
segments(x0 = xleft, y0 = min(app_uptake_tracked$y_pos[app_uptake_tracked$category == "Motivational factors"]), y1 = max(app_uptake_tracked$y_pos[app_uptake_tracked$category == "Motivational factors"]))
par(xpd=FALSE)
dev.off()




# plot tracked app uptake, reported uptake, before and after treatments
pdf(file="../figures/app-tracked-rep-covariates-beforeafter.pdf", height = 12, width = 10, family="Helvetica")
par(oma=c(0,0,0,0))
par(mar=c(3,14,2.5,1))
#par(xaxs = "i")
layout(matrix(c(1, 2, 3, 3), ncol = 2, byrow = TRUE), widths = c(.61, .39), heights = c(1, .03))
# plot tracked app uptake
plot(app_uptake_tracked$prop, 
     app_uptake_tracked$y_pos +.15,
     col = app_uptake_tracked$col,
     xlim = c(0, .8),
     ylim = c(0.5, max(app_uptake_tracked$y_pos) + .5),
     yaxs="i", 
     yaxt = "n", 
     xaxt = "n",
     ylab = "",
     xlab = "")
# grid
#abline(h = app_uptake_tracked$y_pos, col = "grey")
abline(h = app_uptake_tracked$max_y_pos + .5, col = "black")
abline(v = seq(0, .8, .1), col = "grey")
abline(v = app_uptake_tracked$prop_mean, col = "black")
abline(v = app_uptake_w3_tracked$prop_mean, col = "black", lty = 2)
# confidence intervals
segments(x0 = app_uptake_tracked$prop_cilo, x1 = app_uptake_tracked$prop_cihi, y0 = app_uptake_tracked$y_pos +.15, col = app_uptake_tracked$col)
segments(x0 = app_uptake_w3_tracked$prop_cilo, x1 = app_uptake_w3_tracked$prop_cihi, y0 = app_uptake_w3_tracked$y_pos -.15, col = app_uptake_w3_tracked$col)
# points
points(app_uptake_tracked$prop, 
       app_uptake_tracked$y_pos +.15, pch = 19, col = app_uptake_tracked$col, bg = app_uptake_tracked$col)
points(app_uptake_w3_tracked$prop, 
       app_uptake_w3_tracked$y_pos -.15, pch = 21, col = app_uptake_w3_tracked$col, bg = "white")
# axes
axis(1, at = seq(0, .8, .1), label = seq(0, .8, .1))
Map(axis, side = 2, at = app_uptake_tracked$y_pos, col.axis = app_uptake_tracked$col, labels = app_uptake_tracked$val_label, las=1)
#axis(2,at=1:3,labels=FALSE)
#axis(2, at = app_uptake_tracked$y_pos, app_uptake_tracked$val_label, las = 1, col.axis = app_uptake_tracked$col)
Map(axis, side = 2, at = unique(app_uptake_tracked$y_pos_cat), col.axis = app_uptake_tracked$col[app_uptake_tracked$first_cat == TRUE], labels = unique(str_replace(app_uptake_tracked$var_label, "Risk behavior:\\n", "")), line = 10, tick = FALSE, las = 1, hadj = 0)
par(xpd=TRUE)
rect(ybottom = max(app_uptake_tracked$y_pos) + .5, ytop = max(app_uptake_tracked$y_pos) + 1.5, xleft = -.032, xright = .832, col = "lightgrey", border = "black")
par(xpd=FALSE)
axis(3, at = 0.4, label = "Tracked app uptake rate, W1 vs. W3", line = -.9, tick = FALSE)
# variable group labels
axis(2, at = mean(app_uptake_tracked$y_pos[app_uptake_tracked$category == "Sociodemographics"]), label = "Sociodemographics", line = 11, tick = FALSE)
axis(2, at = mean(app_uptake_tracked$y_pos[app_uptake_tracked$category == "Risk status and behavior"]), label = "Risk status and behavior", line = 11, tick = FALSE)
axis(2, at = mean(app_uptake_tracked$y_pos[app_uptake_tracked$category == "Motivational factors"]), label = "Attitudes", line = 11, tick = FALSE)
par(xpd=TRUE)
xleft <- -.495
segments(x0 = xleft, y0 = min(app_uptake_tracked$y_pos[app_uptake_tracked$category == "Sociodemographics"]), y1 = max(app_uptake_tracked$y_pos[app_uptake_tracked$category == "Sociodemographics"]))
segments(x0 = xleft, y0 = min(app_uptake_tracked$y_pos[app_uptake_tracked$category == "Risk status and behavior"]), y1 = max(app_uptake_tracked$y_pos[app_uptake_tracked$category == "Risk status and behavior"]))
segments(x0 = xleft, y0 = min(app_uptake_tracked$y_pos[app_uptake_tracked$category == "Motivational factors"]), y1 = max(app_uptake_tracked$y_pos[app_uptake_tracked$category == "Motivational factors"]))
par(xpd=FALSE)

# plot reported uptake
par(mar=c(3,.5,2.5,1))
plot(app_uptake_reported$prop, 
     app_uptake_reported$y_pos + .15,
     xlim = c(0, .8),
     ylim = c(0.5, max(app_uptake_reported$y_pos) + .5),
     yaxs="i", 
     yaxt = "n", 
     xaxt = "n",
     ylab = "",
     xlab = "")
# grid
#abline(h = app_uptake_reported$y_pos, col = "grey")
abline(h = app_uptake_reported$max_y_pos + .5, col = "black")
abline(v = seq(0, .8, .1), col = "grey")
abline(v = app_uptake_reported$prop_mean, col = "black")
abline(v = app_uptake_w3_reported$prop_mean, col = "black", lty = 2)

# confidence intervals
segments(x0 = app_uptake_reported$prop_cilo, x1 = app_uptake_reported$prop_cihi, y0 = app_uptake_reported$y_pos +.15, col = app_uptake_reported$col)
segments(x0 = app_uptake_w3_reported$prop_cilo, x1 = app_uptake_w3_reported$prop_cihi, y0 = app_uptake_w3_reported$y_pos -.15, col = app_uptake_w3_reported$col)
# points
points(app_uptake_reported$prop, 
       app_uptake_reported$y_pos +.15, pch = 19, col = app_uptake_reported$col, bg = app_uptake_reported$col)
points(app_uptake_w3_reported$prop, 
       app_uptake_w3_reported$y_pos -.15, pch = 21, col = app_uptake_w3_reported$col, bg = "white")
# axes
axis(1, at = seq(0, .8, .1), label = seq(0, .8, .1))
par(xpd=TRUE)
rect(ybottom = max(app_uptake_tracked$y_pos) + .5, ytop = max(app_uptake_tracked$y_pos) + 1.5, xleft = -.032, xright = .832, col = "lightgrey", border = "black")
par(xpd=FALSE)
axis(3, at = 0.4, label = "Reported app uptake rate, W1 vs. W3", line = -.9, tick = FALSE)

# legend
par(mai=c(0,0,0,0))
plot.new()
legend(x="center", x.intersp = .2, legend=c("Wave 1", "Wave 3"),
       col = c("#2166ac", "#2166ac"), bg = c("#2166ac","white"), lty=c(1, 1), pch = c(19, 1), pt.cex = 1, cex = 1, horiz = TRUE,  bty = "n")
dev.off()



# plot bluetooth usage, avg daily usage, before and after treatments
pdf(file="../figures/app-bt-daily-covariates-beforeafter.pdf", height = 12, width = 10, family="Helvetica")
par(oma=c(0,0,0,0))
par(mar=c(3,14,2.5,1))
#par(xaxs = "i")
layout(matrix(c(1, 2, 3, 3), ncol = 2, byrow = TRUE), widths = c(.61, .39), heights = c(1, .03))
# plot app uptake
plot(bt_usage_tracked$prop, 
     bt_usage_tracked$y_pos +.15,
     col = bt_usage_tracked$col,
     xlim = c(0.3, 1),
     ylim = c(0.5, max(bt_usage_tracked$y_pos) + .5),
     yaxs="i", 
     yaxt = "n", 
     xaxt = "n",
     ylab = "",
     xlab = "")
# grid
#abline(h = bt_usage_tracked$y_pos, col = "grey")
abline(h = bt_usage_tracked$max_y_pos + .5, col = "black")
abline(v = seq(.3, 1, .1), col = "grey")
abline(v = bt_usage_tracked$prop_mean, col = "black")
abline(v = bt_usage_w3_tracked$prop_mean, col = "black", lty = 2)
# confidence intervals
segments(x0 = bt_usage_tracked$prop_cilo, x1 = bt_usage_tracked$prop_cihi, y0 = bt_usage_tracked$y_pos +.15, col = bt_usage_tracked$col)
segments(x0 = bt_usage_w3_tracked$prop_cilo, x1 = bt_usage_w3_tracked$prop_cihi, y0 = bt_usage_w3_tracked$y_pos -.15, col = bt_usage_w3_tracked$col)
# points
points(bt_usage_tracked$prop, 
       bt_usage_tracked$y_pos +.15, pch = 19, col = bt_usage_tracked$col, bg = bt_usage_tracked$col)
points(bt_usage_w3_tracked$prop, 
       bt_usage_w3_tracked$y_pos -.15, pch = 21, col = bt_usage_w3_tracked$col, bg = "white")
# axes
axis(1, at = seq(.3, 1, .1), label = seq(.3, 1, .1))
Map(axis, side = 2, at = bt_usage_tracked$y_pos, col.axis = bt_usage_tracked$col, labels = bt_usage_tracked$val_label, las=1)
#axis(2,at=1:3,labels=FALSE)
#axis(2, at = bt_usage_tracked$y_pos, bt_usage_tracked$val_label, las = 1, col.axis = bt_usage_tracked$col)
Map(axis, side = 2, at = unique(bt_usage_tracked$y_pos_cat), col.axis = bt_usage_tracked$col[bt_usage_tracked$first_cat == TRUE], labels = unique(str_replace(bt_usage_tracked$var_label, "Risk behavior:\\n", "")), line = 10, tick = FALSE, las = 1, hadj = 0)
par(xpd=TRUE)
rect(ybottom = max(bt_usage_tracked$y_pos) + .5, ytop = max(bt_usage_tracked$y_pos) + 1.5, xleft = 0.272, xright = 1.028, col = "lightgrey", border = "black")
par(xpd=FALSE)
axis(3, at = 0.65, label = "Bluetooth always-on rate (reported)", line = -.9, tick = FALSE)
# variable group labels
axis(2, at = mean(bt_usage_tracked$y_pos[bt_usage_tracked$category == "Sociodemographics"]), label = "Sociodemographics", line = 11, tick = FALSE)
axis(2, at = mean(bt_usage_tracked$y_pos[bt_usage_tracked$category == "Risk status and behavior"]), label = "Risk status and behavior", line = 11, tick = FALSE)
axis(2, at = mean(bt_usage_tracked$y_pos[bt_usage_tracked$category == "Motivational factors"]), label = "Attitudes", line = 11, tick = FALSE)
par(xpd=TRUE)
xleft <- -.13
segments(x0 = xleft, y0 = min(bt_usage_tracked$y_pos[bt_usage_tracked$category == "Sociodemographics"]), y1 = max(bt_usage_tracked$y_pos[bt_usage_tracked$category == "Sociodemographics"]))
segments(x0 = xleft, y0 = min(bt_usage_tracked$y_pos[bt_usage_tracked$category == "Risk status and behavior"]), y1 = max(bt_usage_tracked$y_pos[bt_usage_tracked$category == "Risk status and behavior"]))
segments(x0 = xleft, y0 = min(bt_usage_tracked$y_pos[bt_usage_tracked$category == "Motivational factors"]), y1 = max(bt_usage_tracked$y_pos[bt_usage_tracked$category == "Motivational factors"]))
par(xpd=FALSE)

# plot daily usage
par(mar=c(3,.5,2.5,1))
plot(daily_usage_tracked$dailyavg, 
     daily_usage_tracked$y_pos + .15,
     xlim = c(0, 4),
     ylim = c(0.5, max(daily_usage_tracked$y_pos) + .5),
     yaxs="i", 
     yaxt = "n", 
     xaxt = "n",
     ylab = "",
     xlab = "")
# grid
#abline(h = daily_usage_tracked$y_pos, col = "grey")
abline(h = daily_usage_tracked$max_y_pos + .5, col = "black")
abline(v = seq(0, 4, .5), col = "grey")
abline(v = daily_usage_tracked$dailyavg_mean, col = "black")
abline(v = daily_usage_w3_tracked$dailyavg_mean, col = "black", lty = 2)

# confidence intervals
segments(x0 = daily_usage_tracked$dailyavg_cilo, x1 = daily_usage_tracked$dailyavg_cihi, y0 = daily_usage_tracked$y_pos +.15, col = daily_usage_tracked$col)
segments(x0 = daily_usage_w3_tracked$dailyavg_cilo, x1 = daily_usage_w3_tracked$dailyavg_cihi, y0 = daily_usage_w3_tracked$y_pos -.15, col = daily_usage_w3_tracked$col)
# points
points(daily_usage_tracked$dailyavg, 
       daily_usage_tracked$y_pos +.15, pch = 19, col = daily_usage_tracked$col, bg = daily_usage_tracked$col)
points(daily_usage_w3_tracked$dailyavg, 
       daily_usage_w3_tracked$y_pos -.15, pch = 21, col = daily_usage_w3_tracked$col, bg = "white")
# axes
axis(1, at = seq(0, 4, .5), label = seq(0, 4, .5))
par(xpd=TRUE)
rect(ybottom = max(daily_usage_tracked$y_pos) + .5, ytop = max(daily_usage_tracked$y_pos) + 1.5, xleft = -.16, xright = 4.16, col = "lightgrey", border = "black")
par(xpd=FALSE)
axis(3, at = 2, label = "Avg. daily app uses (tracked)", line = -.9, tick = FALSE)

# legend
par(mai=c(0,0,0,0))
plot.new()
legend(x="center", x.intersp = .2, legend=c("Wave 1", "Wave 3"),
       col = c("#2166ac", "#2166ac"), bg = c("#2166ac","white"), lty=c(1, 1), pch = c(19, 1), pt.cex = 1, cex = 1, horiz = TRUE,  bty = "n")
dev.off()










# cumulative time-series of first app usage, together with survey timings -------------------

# field times
(fieldtime_w1 <- range(ger_df_wide$StartDate.1, na.rm = TRUE))
(fieldtime_w2 <- range(ger_df_wide$StartDate.2, na.rm = TRUE))
(fieldtime_w3 <- range(ger_df_wide$StartDate.3, na.rm = TRUE))

# identify unique obs
apple_remove == TRUE
if(apple_remove == TRUE){
tracking_df_unique <- distinct(tracking_df_all, id, .keep_all = TRUE) %>% filter(device_manufacturer != "Apple")
}else{
  tracking_df_unique <- distinct(tracking_df_all, id, .keep_all = TRUE) 
  tracking_df_unique$apple <- tracking_df_unique$device_manufacturer == "Apple"
}

# merge treatment status to tracking data
treat_df <- dplyr::select(ger_df_wide, id, treat.1, treat_incent.2, app_install.2, wave.1, wave.2, wave.3)
tracking_df_unique <- merge(tracking_df_unique, treat_df, by.x = "id", by.y = "id")

# compute cumulative time series
generate_ts <- function(data) {
  install_dates <- sort(data[,"track_date_first_install"][data[,"track_date_first_install"] != as.Date("9999-01-01")])
  dates <- seq(as.Date("2020/06/16"), as.Date("2020/09/22"), by = "day") # replace with 2020/11/02 for full tracking data or with 2020/09/22 for study time frame
  count <- as.numeric()
  for(i in seq_along(dates)) {
    count[i] <- sum(install_dates <= dates[i])
  }
  tracking_ts <- data.frame(dates, count, prop = count/nrow(data)) %>% mutate(
    prop_cilo = prop - 1.39*sqrt((prop*(1-prop))/nrow(data)),
    prop_cihi = prop + 1.39*sqrt((prop*(1-prop))/nrow(data)),
    nobs = nrow(data)
  )
  tracking_ts
}

tracking_ts_combined <- generate_ts(tracking_df_unique)
tracking_ts_trackingonly <- generate_ts(filter(tracking_df_unique, survey_indicator == "no survey"))
tracking_ts_surveyplus <- generate_ts(filter(tracking_df_unique, survey_indicator == "survey"))
tracking_ts_surveyplus_message_treatment <- generate_ts(filter(tracking_df_unique, survey_indicator == "survey" & treat.1 != "control"))
tracking_ts_surveyplus_message_control <- generate_ts(filter(tracking_df_unique, survey_indicator == "survey" & treat.1 == "control"))
tracking_ts_surveyplus_incent_treatment <- generate_ts(filter(tracking_df_unique, survey_indicator == "survey" & (treat_incent.2 %in% c("EUR 1", "EUR 2", "EUR 3"))))
tracking_ts_surveyplus_incent_control <- generate_ts(filter(tracking_df_unique, survey_indicator == "survey" & treat_incent.2 == "Control"))

tracking_ts_surveyplus_message_treatment_incent_treatment <- generate_ts(filter(tracking_df_unique, survey_indicator == "survey" & treat.1 != "control" & (treat_incent.2 %in% c("EUR 1", "EUR 2", "EUR 3"))))
tracking_ts_surveyplus_message_treatment_incent_control <- generate_ts(filter(tracking_df_unique, survey_indicator == "survey" & treat.1 != "control" & treat_incent.2 == "Control"))
tracking_ts_surveyplus_message_treatment_incent_na <- generate_ts(filter(tracking_df_unique, survey_indicator == "survey" & treat.1 != "control" & is.na(treat_incent.2)))
tracking_ts_surveyplus_message_control_incent_treatment <- generate_ts(filter(tracking_df_unique, survey_indicator == "survey" & treat.1 == "control" & (treat_incent.2 %in% c("EUR 1", "EUR 2", "EUR 3"))))
tracking_ts_surveyplus_message_control_incent_control <- generate_ts(filter(tracking_df_unique, survey_indicator == "survey" & treat.1 == "control" & treat_incent.2 == "Control"))
tracking_ts_surveyplus_message_control_incent_na <- generate_ts(filter(tracking_df_unique, survey_indicator == "survey" & treat.1 == "control" & is.na(treat_incent.2)))

tracking_ts_surveyplus_incent_treatment_noprevinstall <- generate_ts(filter(tracking_df_unique, survey_indicator == "survey" & (treat_incent.2 %in% c("EUR 1", "EUR 2", "EUR 3") & app_install.2 != 3)))
tracking_ts_surveyplus_incent_control_noprevinstall <- generate_ts(filter(tracking_df_unique, survey_indicator == "survey" & treat_incent.2 == "Control" & app_install.2 != 3))

if(apple_remove == FALSE){
tracking_ts_surveyplus_apple <- generate_ts(filter(tracking_df_unique, apple == TRUE & survey_indicator == "survey"))
tracking_ts_trackingonly_apple <- generate_ts(filter(tracking_df_unique, apple == TRUE & survey_indicator == "no survey"))
tracking_ts_surveyplus_noapple <- generate_ts(filter(tracking_df_unique, apple == FALSE & survey_indicator == "survey"))
tracking_ts_trackingonly_noapple <- generate_ts(filter(tracking_df_unique, apple == FALSE & survey_indicator == "no survey"))
}


# plot
pdf(file="../figures/app-usage-tracking-timeseries.pdf", height = 5, width = 9, family="Helvetica")
par(oma=c(0,0,0,0))
par(mar=c(2.5,2,1,1))
cols <- c('#b2182b','#e08214','#2166ac','#4393c3','#1a1a1a')
# baseline TS
plot(tracking_ts_surveyplus_message_treatment$dates, tracking_ts_surveyplus_message_treatment$prop, type = "l", xaxt = "n", yaxt = "n", xaxs="i", yaxs="i", ylim = c(0, .6), xlim = range(tracking_ts_surveyplus$dates), xlab = NA, ylab = "App adoption rate")
# add grid
datelabels <-  tracking_ts_surveyplus$dates[seq(1, length(tracking_ts_surveyplus$dates), 7)]
abline(h = seq(0, .6, .1), col = "grey80", lty = 3)
abline(v = datelabels, col = "grey80", lty = 3)
# label waves
rect(xleft = as.Date(fieldtime_w1[1]), xright = as.Date(fieldtime_w1[2]), ybottom = 0, ytop = 1, col = rgb(0.5,0.5,0.5, 1/4), border = NA)
rect(xleft = as.Date(fieldtime_w2[1]), xright = as.Date(fieldtime_w2[2]), ybottom = 0, ytop = 1, col = rgb(0.5,0.5,0.5, 1/4), border = NA)
rect(xleft = as.Date(fieldtime_w3[1]), xright = as.Date(fieldtime_w3[2]), ybottom = 0, ytop = 1, col = rgb(0.5,0.5,0.5, 1/4), border = NA)
text(mean(c(as.Date(fieldtime_w1[1]), as.Date(fieldtime_w1[2]))), .56, "Wave 1\n(Message\nTreatment)", cex = .7)
text(mean(c(as.Date(fieldtime_w2[1]), as.Date(fieldtime_w2[2]))), .56, "Wave 2\n(Incentive\nTreatment)", cex = .7)
text(mean(c(as.Date(fieldtime_w3[1]), as.Date(fieldtime_w3[2]))), .58, "Wave 3", cex = .7)
# add CIs as polygons
  # message treatment
CI.x <- c(tracking_ts_surveyplus_message_treatment$dates, rev(tracking_ts_surveyplus_message_treatment$dates)) # polygons are drawn clockwise
CI.y <- c(tracking_ts_surveyplus_message_treatment$prop_cihi, rev(tracking_ts_surveyplus_message_treatment$prop_cilo))
CI.col <- adjustcolor(cols[1],alpha.f=0.25)
polygon(CI.x, CI.y, col=CI.col, border=NA)
  # message control
CI.x <- c(tracking_ts_surveyplus_message_control$dates, rev(tracking_ts_surveyplus_message_control$dates)) # polygons are drawn clockwise
CI.y <- c(tracking_ts_surveyplus_message_control$prop_cihi, rev(tracking_ts_surveyplus_message_control$prop_cilo))
CI.col <- adjustcolor(cols[2],alpha.f=0.25)
polygon(CI.x, CI.y, col=CI.col, border=NA)
# incent treatment
CI.x <- c(tracking_ts_surveyplus_incent_treatment$dates, rev(tracking_ts_surveyplus_incent_treatment$dates)) # polygons are drawn clockwise
CI.y <- c(tracking_ts_surveyplus_incent_treatment$prop_cihi, rev(tracking_ts_surveyplus_incent_treatment$prop_cilo))
CI.col <- adjustcolor(cols[3],alpha.f=0.25)
polygon(CI.x, CI.y, col=CI.col, border=NA)
# incent control
CI.x <- c(tracking_ts_surveyplus_incent_control$dates, rev(tracking_ts_surveyplus_incent_control$dates)) # polygons are drawn clockwise
CI.y <- c(tracking_ts_surveyplus_incent_control$prop_cihi, rev(tracking_ts_surveyplus_incent_control$prop_cilo))
CI.col <- adjustcolor(cols[4],alpha.f=0.25)
polygon(CI.x, CI.y, col=CI.col, border=NA)
# tracking-only sample
CI.x <- c(tracking_ts_trackingonly$dates, rev(tracking_ts_trackingonly$dates)) # polygons are drawn clockwise
CI.y <- c(tracking_ts_trackingonly$prop_cihi, rev(tracking_ts_trackingonly$prop_cilo))
CI.col <- adjustcolor(cols[5],alpha.f=0.25)
polygon(CI.x, CI.y, col=CI.col, border=NA)
# add main TS
lines(tracking_ts_surveyplus_message_treatment$dates, tracking_ts_surveyplus_message_treatment$prop, col = cols[1], lty = 1, lwd = 2)
lines(tracking_ts_surveyplus_message_control$dates, tracking_ts_surveyplus_message_control$prop, col = cols[2], lty = 2, lwd = 2)
lines(tracking_ts_surveyplus_incent_treatment$dates, tracking_ts_surveyplus_incent_treatment$prop, col = cols[3], lty = 1, lwd = 2)
lines(tracking_ts_surveyplus_incent_control$dates, tracking_ts_surveyplus_incent_control$prop, col = cols[4], lty = 2, lwd = 2)
lines(tracking_ts_trackingonly$dates, tracking_ts_trackingonly$prop, col = cols[5], lty = 1, lwd = 2)
# define dates
dates_points_surveyplus_message_treatment <- filter(tracking_ts_surveyplus_message_treatment, dates %in% as.Date(c(fieldtime_w1, fieldtime_w2, fieldtime_w3)))
dates_points_surveyplus_message_control <- filter(tracking_ts_surveyplus_message_control, dates %in% as.Date(c(fieldtime_w1, fieldtime_w2, fieldtime_w3)))
dates_points_surveyplus_incent_treatment <- filter(tracking_ts_surveyplus_incent_treatment, dates %in% as.Date(c(fieldtime_w1, fieldtime_w2, fieldtime_w3)))
dates_points_surveyplus_incent_control <- filter(tracking_ts_surveyplus_incent_control, dates %in% as.Date(c(fieldtime_w1, fieldtime_w2, fieldtime_w3)))
dates_points_trackingonly <- filter(tracking_ts_trackingonly, dates %in% as.Date(c(fieldtime_w1, fieldtime_w2, fieldtime_w3)))
# plot points
points(dates_points_surveyplus_message_treatment$dates, dates_points_surveyplus_message_treatment$prop, pch = 21, col = "black", bg = cols[1])
points(dates_points_surveyplus_message_control$dates, dates_points_surveyplus_message_control$prop, pch = 21, col = "black", bg = cols[2])
points(dates_points_surveyplus_incent_treatment$dates, dates_points_surveyplus_incent_treatment$prop, pch = 21, col = "black", bg = cols[3])
points(dates_points_surveyplus_incent_control$dates, dates_points_surveyplus_incent_control$prop, pch = 21, col = "black", bg = cols[4])
points(dates_points_trackingonly$dates, dates_points_trackingonly$prop, pch = 21, col = "black", bg = cols[5])
# plot text labels
text(dates_points_surveyplus_message_treatment$dates, dates_points_surveyplus_message_treatment$prop, str_replace(sprintf("%0.2f", round(dates_points_surveyplus_message_treatment$prop, 2)), "^0", ""), pos = 3, cex = .7, col = cols[1])
text(dates_points_surveyplus_message_control$dates, dates_points_surveyplus_message_control$prop, str_replace(sprintf("%0.2f", round(dates_points_surveyplus_message_control$prop, 2)), "^0", ""), pos = 1, cex = .7, col = cols[2])
text(dates_points_surveyplus_incent_treatment$dates, dates_points_surveyplus_incent_treatment$prop, str_replace(sprintf("%0.2f", round(dates_points_surveyplus_incent_treatment$prop, 2)), "^0", ""), pos = 1, cex = .7, col = cols[3])
text(dates_points_surveyplus_incent_control$dates, dates_points_surveyplus_incent_control$prop, str_replace(sprintf("%0.2f", round(dates_points_surveyplus_incent_control$prop, 2)), "^0", ""), pos = 3, cex = .7, col = cols[4])
text(dates_points_trackingonly$dates, dates_points_trackingonly$prop, str_replace(sprintf("%0.2f", round(dates_points_trackingonly$prop, 2)), "^0", ""), col = cols[5], pos = 1, cex = .7)
# add axis labels
axis(1, datelabels, format(datelabels, "%b %d"), cex.axis = .7)
axis(2, seq(0, .6, .1), seq(0, .6, .1), las = 1, cex.axis = .7)
# add legend
legend("topleft", 
       y.intersp = 1.5,
       legend = c(paste0("Survey-tracking, message treatment", " (n = ", tracking_ts_surveyplus_message_treatment$nobs[1], ")"), 
                  paste0("Survey-tracking, message control", " (n = ", tracking_ts_surveyplus_message_control$nobs[1], ")"),
                  paste0("Survey-tracking, incentive treatment", " (n = ", tracking_ts_surveyplus_incent_treatment$nobs[1], ")"),
                  paste0("Survey-tracking, incentive control", " (n = ", tracking_ts_surveyplus_incent_control$nobs[1], ")"),
                  paste0("Tracking-only", " (n = ", tracking_ts_trackingonly$nobs[1], ")")
       ), 
       col = cols, 
       lty = c(1, 2, 1, 2, 1), 
       lwd = 2,
       bty = "n", 
       pt.cex = 2, 
       cex = .7, 
       text.col = cols, 
       horiz = F)
dev.off()


# plot, split by experiment
pdf(file="../figures/app-usage-tracking-timeseries-split.pdf", height = 7, width = 8, family="Helvetica")
par(oma=c(0,3,0,0))
par(mar=c(.2,1,2,1))
layout(matrix(c(1, 2), nrow = 2), heights = c(.45, .55))
cols <- c('#b2182b','#e08214','#2166ac','#4393c3','#1a1a1a')
# MESSAGE TREATMENT
# baseline TS
plot(tracking_ts_surveyplus_message_treatment$dates, tracking_ts_surveyplus_message_treatment$prop, type = "l", xaxt = "n", yaxt = "n", xaxs="i", yaxs="i", ylim = c(0, .7), xlim = range(tracking_ts_surveyplus$dates), xlab = NA, ylab = "")
# label waves
rect(xleft = as.Date(fieldtime_w1[1]), xright = as.Date(fieldtime_w1[2]), ybottom = 0, ytop = 1, col = rgb(0.5,0.5,0.5, 1/4), border = NA)
rect(xleft = as.Date(fieldtime_w2[1]), xright = as.Date(fieldtime_w2[2]), ybottom = 0, ytop = 1, col = rgb(0.5,0.5,0.5, 1/4), border = NA)
rect(xleft = as.Date(fieldtime_w3[1]), xright = as.Date(fieldtime_w3[2]), ybottom = 0, ytop = 1, col = rgb(0.5,0.5,0.5, 1/4), border = NA)
text(mean(c(as.Date(fieldtime_w1[1]), as.Date(fieldtime_w1[2]))), .63, "Wave 1\n(Message exp.)", cex = .7)
text(mean(c(as.Date(fieldtime_w2[1]), as.Date(fieldtime_w2[2]))), .63, "Wave 2\n(Incentive exp.)", cex = .7)
text(mean(c(as.Date(fieldtime_w3[1]), as.Date(fieldtime_w3[2]))), .65, "Wave 3", cex = .7)
# add grid
datelabels <-  tracking_ts_surveyplus$dates[seq(1, length(tracking_ts_surveyplus$dates), 7)]
abline(h = seq(0, .7, .1), col = "grey80", lty = 3)
abline(v = datelabels, col = "grey80", lty = 3)
# message treatment
CI.x <- c(tracking_ts_surveyplus_message_treatment$dates, rev(tracking_ts_surveyplus_message_treatment$dates))
CI.y <- c(tracking_ts_surveyplus_message_treatment$prop_cihi, rev(tracking_ts_surveyplus_message_treatment$prop_cilo))
CI.col <- adjustcolor(cols[1],alpha.f=0.25)
polygon(CI.x, CI.y, col=CI.col, border=NA)
# message control
CI.x <- c(tracking_ts_surveyplus_message_control$dates, rev(tracking_ts_surveyplus_message_control$dates))
CI.y <- c(tracking_ts_surveyplus_message_control$prop_cihi, rev(tracking_ts_surveyplus_message_control$prop_cilo))
CI.col <- adjustcolor(cols[2],alpha.f=0.25)
polygon(CI.x, CI.y, col=CI.col, border=NA)
# tracking-only sample
CI.x <- c(tracking_ts_trackingonly$dates, rev(tracking_ts_trackingonly$dates)) # polygons are drawn clockwise
CI.y <- c(tracking_ts_trackingonly$prop_cihi, rev(tracking_ts_trackingonly$prop_cilo))
CI.col <- adjustcolor(cols[5],alpha.f=0.25)
polygon(CI.x, CI.y, col=CI.col, border=NA)
# add main TS
lines(tracking_ts_surveyplus_message_treatment$dates, tracking_ts_surveyplus_message_treatment$prop, col = cols[1], lty = 1, lwd = 2)
lines(tracking_ts_surveyplus_message_control$dates, tracking_ts_surveyplus_message_control$prop, col = cols[2], lty = 2, lwd = 2)
lines(tracking_ts_trackingonly$dates, tracking_ts_trackingonly$prop, col = cols[5], lty = 1, lwd = 2)
# define dates
dates_points_surveyplus_message_treatment <- filter(tracking_ts_surveyplus_message_treatment, dates %in% as.Date(c(fieldtime_w1, fieldtime_w2, fieldtime_w3)))
dates_points_surveyplus_message_control <- filter(tracking_ts_surveyplus_message_control, dates %in% as.Date(c(fieldtime_w1, fieldtime_w2, fieldtime_w3)))
dates_points_trackingonly <- filter(tracking_ts_trackingonly, dates %in% as.Date(c(fieldtime_w1, fieldtime_w2, fieldtime_w3)))
# plot points
points(dates_points_surveyplus_message_treatment$dates, dates_points_surveyplus_message_treatment$prop, pch = 21, col = "black", bg = cols[1])
points(dates_points_surveyplus_message_control$dates, dates_points_surveyplus_message_control$prop, pch = 21, col = "black", bg = cols[2])
points(dates_points_trackingonly$dates, dates_points_trackingonly$prop, pch = 21, col = "black", bg = cols[5])
# plot text labels
text(dates_points_surveyplus_message_treatment$dates, dates_points_surveyplus_message_treatment$prop, str_replace(sprintf("%0.2f", round(dates_points_surveyplus_message_treatment$prop, 2)), "^0", ""), pos = 3, cex = .7, col = cols[1])
text(dates_points_surveyplus_message_control$dates, dates_points_surveyplus_message_control$prop, str_replace(sprintf("%0.2f", round(dates_points_surveyplus_message_control$prop, 2)), "^0", ""), pos = 1, cex = .7, col = cols[2])
text(dates_points_trackingonly$dates, dates_points_trackingonly$prop, str_replace(sprintf("%0.2f", round(dates_points_trackingonly$prop, 2)), "^0", ""), col = cols[5], pos = 1, cex = .7)
# add axis labels
axis(2, seq(0, .7, .1), seq(0, .7, .1), las = 1, cex.axis = .7)
# add legend
legend("topleft", 
       y.intersp = 1.5,
       legend = c(paste0("Survey-tracking, treatment", " (n = ", tracking_ts_surveyplus_message_treatment$nobs[1], ")"), 
                  paste0("Survey-tracking, control", " (n = ", tracking_ts_surveyplus_message_control$nobs[1], ")"),
                  paste0("Tracking-only", " (n = ", tracking_ts_trackingonly$nobs[1], ")")
       ), 
       col = cols[c(1,2,5)], 
       lty = c(1, 2, 1), 
       lwd = 2,
       bty = "n", 
       pt.cex = 2, 
       cex = .7, 
       text.col = cols[c(1,2,5)], 
       horiz = F)
# title label
par(xpd=TRUE)
rect(ybottom = .7, ytop = .77, xleft = range(tracking_ts_surveyplus$dates)[1], xright = range(tracking_ts_surveyplus$dates)[2], col = "lightgrey", border = "black")
par(xpd=FALSE)
axis(3, at = tracking_ts_surveyplus$dates[round(length(tracking_ts_surveyplus$dates)/2)], label = "Message experiment groups", line = -.8, tick = FALSE)

# INCENTIVE TREATMENT
par(mar=c(2.5,1,2,1))
# baseline TS
plot(tracking_ts_surveyplus_incent_treatment$dates, tracking_ts_surveyplus_incent_treatment$prop, type = "l", xaxt = "n", yaxt = "n", xaxs="i", yaxs="i", ylim = c(0, .7), xlim = range(tracking_ts_surveyplus$dates), xlab = NA, ylab = "")
# waves
rect(xleft = as.Date(fieldtime_w1[1]), xright = as.Date(fieldtime_w1[2]), ybottom = 0, ytop = 1, col = rgb(0.5,0.5,0.5, 1/4), border = NA)
rect(xleft = as.Date(fieldtime_w2[1]), xright = as.Date(fieldtime_w2[2]), ybottom = 0, ytop = 1, col = rgb(0.5,0.5,0.5, 1/4), border = NA)
rect(xleft = as.Date(fieldtime_w3[1]), xright = as.Date(fieldtime_w3[2]), ybottom = 0, ytop = 1, col = rgb(0.5,0.5,0.5, 1/4), border = NA)
text(mean(c(as.Date(fieldtime_w1[1]), as.Date(fieldtime_w1[2]))), .63, "Wave 1\n(Message exp.)", cex = .7)
text(mean(c(as.Date(fieldtime_w2[1]), as.Date(fieldtime_w2[2]))), .63, "Wave 2\n(Incentive exp.)", cex = .7)
text(mean(c(as.Date(fieldtime_w3[1]), as.Date(fieldtime_w3[2]))), .65, "Wave 3", cex = .7)
# add grid
datelabels <-  tracking_ts_surveyplus$dates[seq(1, length(tracking_ts_surveyplus$dates), 7)]
abline(h = seq(0, .7, .1), col = "grey80", lty = 3)
abline(v = datelabels, col = "grey80", lty = 3)
# add CIs as polygons
# incent treatment
CI.x <- c(tracking_ts_surveyplus_incent_treatment$dates, rev(tracking_ts_surveyplus_incent_treatment$dates)) # polygons are drawn clockwise
CI.y <- c(tracking_ts_surveyplus_incent_treatment$prop_cihi, rev(tracking_ts_surveyplus_incent_treatment$prop_cilo))
CI.col <- adjustcolor(cols[3],alpha.f=0.25)
polygon(CI.x, CI.y, col=CI.col, border=NA)
# incent control
CI.x <- c(tracking_ts_surveyplus_incent_control$dates, rev(tracking_ts_surveyplus_incent_control$dates)) # polygons are drawn clockwise
CI.y <- c(tracking_ts_surveyplus_incent_control$prop_cihi, rev(tracking_ts_surveyplus_incent_control$prop_cilo))
CI.col <- adjustcolor(cols[4],alpha.f=0.25)
polygon(CI.x, CI.y, col=CI.col, border=NA)
# add main TS
lines(tracking_ts_surveyplus_incent_treatment$dates, tracking_ts_surveyplus_incent_treatment$prop, col = cols[3], lty = 1, lwd = 2)
lines(tracking_ts_surveyplus_incent_control$dates, tracking_ts_surveyplus_incent_control$prop, col = cols[4], lty = 2, lwd = 2)
# define dates
dates_points_surveyplus_incent_treatment <- filter(tracking_ts_surveyplus_incent_treatment, dates %in% as.Date(c(fieldtime_w1, fieldtime_w2, fieldtime_w3)))
dates_points_surveyplus_incent_control <- filter(tracking_ts_surveyplus_incent_control, dates %in% as.Date(c(fieldtime_w1, fieldtime_w2, fieldtime_w3)))
# plot points
points(dates_points_surveyplus_incent_treatment$dates, dates_points_surveyplus_incent_treatment$prop, pch = 21, col = "black", bg = cols[3])
points(dates_points_surveyplus_incent_control$dates, dates_points_surveyplus_incent_control$prop, pch = 21, col = "black", bg = cols[4])
# plot text labels
text(dates_points_surveyplus_incent_treatment$dates, dates_points_surveyplus_incent_treatment$prop, str_replace(sprintf("%0.2f", round(dates_points_surveyplus_incent_treatment$prop, 2)), "^0", ""), pos = 1, cex = .7, col = cols[3])
text(dates_points_surveyplus_incent_control$dates, dates_points_surveyplus_incent_control$prop, str_replace(sprintf("%0.2f", round(dates_points_surveyplus_incent_control$prop, 2)), "^0", ""), pos = 3, cex = .7, col = cols[4])
# add axis labels
axis(1, datelabels, format(datelabels, "%b %d"), cex.axis = .7)
axis(2, seq(0, .7, .1), seq(0, .7, .1), las = 1, cex.axis = .7)
# add legend
legend("topleft", 
       y.intersp = 1.5,
       legend = c(paste0("Survey-tracking, treatment", " (n = ", tracking_ts_surveyplus_incent_treatment$nobs[1], ")"),
                  paste0("Survey-tracking, control", " (n = ", tracking_ts_surveyplus_incent_control$nobs[1], ")")
       ), 
       col = cols[3:5], 
       lty = c(1, 2,1), 
       lwd = 2,
       bty = "n", 
       pt.cex = 2, 
       cex = .7, 
       text.col = cols[3:5], 
       horiz = F)
# overall axis label
mtext("App adoption rate", 2, 2, outer=TRUE, las=0)
# title label
par(xpd=TRUE)
rect(ybottom = .7, ytop = .77, xleft = range(tracking_ts_surveyplus$dates)[1], xright = range(tracking_ts_surveyplus$dates)[2], col = "lightgrey", border = "black")
par(xpd=FALSE)
axis(3, at = tracking_ts_surveyplus$dates[round(length(tracking_ts_surveyplus$dates)/2)], label = "Incentive experiment groups", line = -.8, tick = FALSE)
dev.off()




# plot, split by experiment, ger
pdf(file="../figures/app-usage-tracking-timeseries-split-ger.pdf", height = 7, width = 8, family="Helvetica")
par(oma=c(0,3,0,0))
par(mar=c(.2,1,2,1))
layout(matrix(c(1, 2), nrow = 2), heights = c(.45, .55))
cols <- c('#b2182b','#e08214','#2166ac','#4393c3','#1a1a1a')
# MESSAGE TREATMENT
# baseline TS
plot(tracking_ts_surveyplus_message_treatment$dates, tracking_ts_surveyplus_message_treatment$prop, type = "l", xaxt = "n", yaxt = "n", xaxs="i", yaxs="i", ylim = c(0, .7), xlim = range(tracking_ts_surveyplus$dates), xlab = NA, ylab = "")
# label waves
rect(xleft = as.Date(fieldtime_w1[1]), xright = as.Date(fieldtime_w1[2]), ybottom = 0, ytop = 1, col = rgb(0.5,0.5,0.5, 1/4), border = NA)
rect(xleft = as.Date(fieldtime_w2[1]), xright = as.Date(fieldtime_w2[2]), ybottom = 0, ytop = 1, col = rgb(0.5,0.5,0.5, 1/4), border = NA)
rect(xleft = as.Date(fieldtime_w3[1]), xright = as.Date(fieldtime_w3[2]), ybottom = 0, ytop = 1, col = rgb(0.5,0.5,0.5, 1/4), border = NA)
text(mean(c(as.Date(fieldtime_w1[1]), as.Date(fieldtime_w1[2]))), .63, "Welle 1\n(Informations-\nexperiment)", cex = .7)
text(mean(c(as.Date(fieldtime_w2[1]), as.Date(fieldtime_w2[2]))), .63, "Welle 2\n(Incentive-\nexperiment)", cex = .7)
text(mean(c(as.Date(fieldtime_w3[1]), as.Date(fieldtime_w3[2]))), .65, "Welle 3", cex = .7)
# add grid
datelabels <-  tracking_ts_surveyplus$dates[seq(1, length(tracking_ts_surveyplus$dates), 7)]
abline(h = seq(0, .7, .1), col = "grey80", lty = 3)
abline(v = datelabels, col = "grey80", lty = 3)
# message treatment
CI.x <- c(tracking_ts_surveyplus_message_treatment$dates, rev(tracking_ts_surveyplus_message_treatment$dates))
CI.y <- c(tracking_ts_surveyplus_message_treatment$prop_cihi, rev(tracking_ts_surveyplus_message_treatment$prop_cilo))
CI.col <- adjustcolor(cols[1],alpha.f=0.25)
polygon(CI.x, CI.y, col=CI.col, border=NA)
# message control
CI.x <- c(tracking_ts_surveyplus_message_control$dates, rev(tracking_ts_surveyplus_message_control$dates))
CI.y <- c(tracking_ts_surveyplus_message_control$prop_cihi, rev(tracking_ts_surveyplus_message_control$prop_cilo))
CI.col <- adjustcolor(cols[2],alpha.f=0.25)
polygon(CI.x, CI.y, col=CI.col, border=NA)
# tracking-only sample
CI.x <- c(tracking_ts_trackingonly$dates, rev(tracking_ts_trackingonly$dates)) # polygons are drawn clockwise
CI.y <- c(tracking_ts_trackingonly$prop_cihi, rev(tracking_ts_trackingonly$prop_cilo))
CI.col <- adjustcolor(cols[5],alpha.f=0.25)
polygon(CI.x, CI.y, col=CI.col, border=NA)
# add main TS
lines(tracking_ts_surveyplus_message_treatment$dates, tracking_ts_surveyplus_message_treatment$prop, col = cols[1], lty = 1, lwd = 2)
lines(tracking_ts_surveyplus_message_control$dates, tracking_ts_surveyplus_message_control$prop, col = cols[2], lty = 2, lwd = 2)
lines(tracking_ts_trackingonly$dates, tracking_ts_trackingonly$prop, col = cols[5], lty = 1, lwd = 2)
# define dates
dates_points_surveyplus_message_treatment <- filter(tracking_ts_surveyplus_message_treatment, dates %in% as.Date(c(fieldtime_w1, fieldtime_w2, fieldtime_w3)))
dates_points_surveyplus_message_control <- filter(tracking_ts_surveyplus_message_control, dates %in% as.Date(c(fieldtime_w1, fieldtime_w2, fieldtime_w3)))
dates_points_trackingonly <- filter(tracking_ts_trackingonly, dates %in% as.Date(c(fieldtime_w1, fieldtime_w2, fieldtime_w3)))
# plot points
points(dates_points_surveyplus_message_treatment$dates, dates_points_surveyplus_message_treatment$prop, pch = 21, col = "black", bg = cols[1])
points(dates_points_surveyplus_message_control$dates, dates_points_surveyplus_message_control$prop, pch = 21, col = "black", bg = cols[2])
points(dates_points_trackingonly$dates, dates_points_trackingonly$prop, pch = 21, col = "black", bg = cols[5])
# plot text labels
text(dates_points_surveyplus_message_treatment$dates, dates_points_surveyplus_message_treatment$prop, str_replace(sprintf("%0.2f", round(dates_points_surveyplus_message_treatment$prop, 2)), "^0", ""), pos = 3, cex = .7, col = cols[1])
text(dates_points_surveyplus_message_control$dates, dates_points_surveyplus_message_control$prop, str_replace(sprintf("%0.2f", round(dates_points_surveyplus_message_control$prop, 2)), "^0", ""), pos = 1, cex = .7, col = cols[2])
text(dates_points_trackingonly$dates, dates_points_trackingonly$prop, str_replace(sprintf("%0.2f", round(dates_points_trackingonly$prop, 2)), "^0", ""), col = cols[5], pos = 1, cex = .7)
# add axis labels
axis(2, seq(0, .7, .1), seq(0, .7, .1), las = 1, cex.axis = .7)
# add legend
legend("topleft", 
       y.intersp = 1.5,
       legend = c(paste0("Survey-tracking, Treatment", " (n = ", tracking_ts_surveyplus_message_treatment$nobs[1], ")"), 
                  paste0("Survey-tracking, Kontrolle", " (n = ", tracking_ts_surveyplus_message_control$nobs[1], ")"),
                  paste0("Tracking-only", " (n = ", tracking_ts_trackingonly$nobs[1], ")")
       ), 
       col = cols[c(1,2,5)], 
       lty = c(1, 2, 1), 
       lwd = 2,
       bty = "n", 
       pt.cex = 2, 
       cex = .7, 
       text.col = cols[c(1,2,5)], 
       horiz = F)
# title label
par(xpd=TRUE)
rect(ybottom = .7, ytop = .77, xleft = range(tracking_ts_surveyplus$dates)[1], xright = range(tracking_ts_surveyplus$dates)[2], col = "lightgrey", border = "black")
par(xpd=FALSE)
axis(3, at = tracking_ts_surveyplus$dates[round(length(tracking_ts_surveyplus$dates)/2)], label = "Informationsexperiment-Gruppen", line = -.8, tick = FALSE)

# INCENTIVE TREATMENT
par(mar=c(2.5,1,2,1))
# baseline TS
plot(tracking_ts_surveyplus_incent_treatment$dates, tracking_ts_surveyplus_incent_treatment$prop, type = "l", xaxt = "n", yaxt = "n", xaxs="i", yaxs="i", ylim = c(0, .7), xlim = range(tracking_ts_surveyplus$dates), xlab = NA, ylab = "")
# waves
rect(xleft = as.Date(fieldtime_w1[1]), xright = as.Date(fieldtime_w1[2]), ybottom = 0, ytop = 1, col = rgb(0.5,0.5,0.5, 1/4), border = NA)
rect(xleft = as.Date(fieldtime_w2[1]), xright = as.Date(fieldtime_w2[2]), ybottom = 0, ytop = 1, col = rgb(0.5,0.5,0.5, 1/4), border = NA)
rect(xleft = as.Date(fieldtime_w3[1]), xright = as.Date(fieldtime_w3[2]), ybottom = 0, ytop = 1, col = rgb(0.5,0.5,0.5, 1/4), border = NA)
text(mean(c(as.Date(fieldtime_w1[1]), as.Date(fieldtime_w1[2]))), .63, "Welle 1\n(Informations-\nexperiment)", cex = .7)
text(mean(c(as.Date(fieldtime_w2[1]), as.Date(fieldtime_w2[2]))), .63, "Welle 2\n(Incentive-\nexperiment)", cex = .7)
text(mean(c(as.Date(fieldtime_w3[1]), as.Date(fieldtime_w3[2]))), .65, "Welle 3", cex = .7)
# add grid
datelabels <-  tracking_ts_surveyplus$dates[seq(1, length(tracking_ts_surveyplus$dates), 7)]
abline(h = seq(0, .7, .1), col = "grey80", lty = 3)
abline(v = datelabels, col = "grey80", lty = 3)
# add CIs as polygons
# incent treatment
CI.x <- c(tracking_ts_surveyplus_incent_treatment$dates, rev(tracking_ts_surveyplus_incent_treatment$dates)) # polygons are drawn clockwise
CI.y <- c(tracking_ts_surveyplus_incent_treatment$prop_cihi, rev(tracking_ts_surveyplus_incent_treatment$prop_cilo))
CI.col <- adjustcolor(cols[3],alpha.f=0.25)
polygon(CI.x, CI.y, col=CI.col, border=NA)
# incent control
CI.x <- c(tracking_ts_surveyplus_incent_control$dates, rev(tracking_ts_surveyplus_incent_control$dates)) # polygons are drawn clockwise
CI.y <- c(tracking_ts_surveyplus_incent_control$prop_cihi, rev(tracking_ts_surveyplus_incent_control$prop_cilo))
CI.col <- adjustcolor(cols[4],alpha.f=0.25)
polygon(CI.x, CI.y, col=CI.col, border=NA)
# add main TS
lines(tracking_ts_surveyplus_incent_treatment$dates, tracking_ts_surveyplus_incent_treatment$prop, col = cols[3], lty = 1, lwd = 2)
lines(tracking_ts_surveyplus_incent_control$dates, tracking_ts_surveyplus_incent_control$prop, col = cols[4], lty = 2, lwd = 2)
# define dates
dates_points_surveyplus_incent_treatment <- filter(tracking_ts_surveyplus_incent_treatment, dates %in% as.Date(c(fieldtime_w1, fieldtime_w2, fieldtime_w3)))
dates_points_surveyplus_incent_control <- filter(tracking_ts_surveyplus_incent_control, dates %in% as.Date(c(fieldtime_w1, fieldtime_w2, fieldtime_w3)))
# plot points
points(dates_points_surveyplus_incent_treatment$dates, dates_points_surveyplus_incent_treatment$prop, pch = 21, col = "black", bg = cols[3])
points(dates_points_surveyplus_incent_control$dates, dates_points_surveyplus_incent_control$prop, pch = 21, col = "black", bg = cols[4])
# plot text labels
text(dates_points_surveyplus_incent_treatment$dates, dates_points_surveyplus_incent_treatment$prop, str_replace(sprintf("%0.2f", round(dates_points_surveyplus_incent_treatment$prop, 2)), "^0", ""), pos = 1, cex = .7, col = cols[3])
text(dates_points_surveyplus_incent_control$dates, dates_points_surveyplus_incent_control$prop, str_replace(sprintf("%0.2f", round(dates_points_surveyplus_incent_control$prop, 2)), "^0", ""), pos = 3, cex = .7, col = cols[4])
# add axis labels
axis(1, datelabels, format(datelabels, "%b %d"), cex.axis = .7)
axis(2, seq(0, .7, .1), seq(0, .7, .1), las = 1, cex.axis = .7)
# add legend
legend("topleft", 
       y.intersp = 1.5,
       legend = c(paste0("Survey-tracking, Treatment", " (n = ", tracking_ts_surveyplus_incent_treatment$nobs[1], ")"),
                  paste0("Survey-tracking, Kontrolle", " (n = ", tracking_ts_surveyplus_incent_control$nobs[1], ")")
       ), 
       col = cols[3:5], 
       lty = c(1, 2,1), 
       lwd = 2,
       bty = "n", 
       pt.cex = 2, 
       cex = .7, 
       text.col = cols[3:5], 
       horiz = F)
# overall axis label
mtext("App-Adoptionsrate", 2, 2, outer=TRUE, las=0)
# title label
par(xpd=TRUE)
rect(ybottom = .7, ytop = .77, xleft = range(tracking_ts_surveyplus$dates)[1], xright = range(tracking_ts_surveyplus$dates)[2], col = "lightgrey", border = "black")
par(xpd=FALSE)
axis(3, at = tracking_ts_surveyplus$dates[round(length(tracking_ts_surveyplus$dates)/2)], label = "Incentiveexperiment-Gruppen", line = -.8, tick = FALSE)
dev.off()



# plot, split by experiment, full tracking period (until Nov 2)
pdf(file="../figures/app-usage-tracking-timeseries-split-fullperiod.pdf", height = 7, width = 8, family="Helvetica")
par(oma=c(0,3,0,0))
par(mar=c(.2,1,2,1))
layout(matrix(c(1, 2), nrow = 2), heights = c(.45, .55))
cols <- c('#b2182b','#e08214','#2166ac','#4393c3','#1a1a1a')
# MESSAGE TREATMENT
# baseline TS
plot(tracking_ts_surveyplus_message_treatment$dates, tracking_ts_surveyplus_message_treatment$prop, type = "l", xaxt = "n", yaxt = "n", xaxs="i", yaxs="i", ylim = c(0, .7), xlim = range(tracking_ts_surveyplus$dates), xlab = NA, ylab = "")
# label waves
rect(xleft = as.Date(fieldtime_w1[1]), xright = as.Date(fieldtime_w1[2]), ybottom = 0, ytop = 1, col = rgb(0.5,0.5,0.5, 1/4), border = NA)
rect(xleft = as.Date(fieldtime_w2[1]), xright = as.Date(fieldtime_w2[2]), ybottom = 0, ytop = 1, col = rgb(0.5,0.5,0.5, 1/4), border = NA)
rect(xleft = as.Date(fieldtime_w3[1]), xright = as.Date(fieldtime_w3[2]), ybottom = 0, ytop = 1, col = rgb(0.5,0.5,0.5, 1/4), border = NA)
text(mean(c(as.Date(fieldtime_w1[1]), as.Date(fieldtime_w1[2]))), .63, "Wave 1\n(Message\ntreatment)", cex = .7)
text(mean(c(as.Date(fieldtime_w2[1]), as.Date(fieldtime_w2[2]))), .63, "Wave 2\n(Incentive\ntreatment)", cex = .7)
text(mean(c(as.Date(fieldtime_w3[1]), as.Date(fieldtime_w3[2]))), .65, "Wave 3", cex = .7)
# add grid
datelabels <-  tracking_ts_surveyplus$dates[seq(1, length(tracking_ts_surveyplus$dates), 7)]
abline(h = seq(0, .7, .1), col = "grey80", lty = 3)
abline(v = datelabels, col = "grey80", lty = 3)
# message treatment
CI.x <- c(tracking_ts_surveyplus_message_treatment$dates, rev(tracking_ts_surveyplus_message_treatment$dates))
CI.y <- c(tracking_ts_surveyplus_message_treatment$prop_cihi, rev(tracking_ts_surveyplus_message_treatment$prop_cilo))
CI.col <- adjustcolor(cols[1],alpha.f=0.25)
polygon(CI.x, CI.y, col=CI.col, border=NA)
# message control
CI.x <- c(tracking_ts_surveyplus_message_control$dates, rev(tracking_ts_surveyplus_message_control$dates))
CI.y <- c(tracking_ts_surveyplus_message_control$prop_cihi, rev(tracking_ts_surveyplus_message_control$prop_cilo))
CI.col <- adjustcolor(cols[2],alpha.f=0.25)
polygon(CI.x, CI.y, col=CI.col, border=NA)
# tracking-only sample
CI.x <- c(tracking_ts_trackingonly$dates, rev(tracking_ts_trackingonly$dates)) # polygons are drawn clockwise
CI.y <- c(tracking_ts_trackingonly$prop_cihi, rev(tracking_ts_trackingonly$prop_cilo))
CI.col <- adjustcolor(cols[5],alpha.f=0.25)
polygon(CI.x, CI.y, col=CI.col, border=NA)
# add main TS
lines(tracking_ts_surveyplus_message_treatment$dates, tracking_ts_surveyplus_message_treatment$prop, col = cols[1], lty = 1, lwd = 2)
lines(tracking_ts_surveyplus_message_control$dates, tracking_ts_surveyplus_message_control$prop, col = cols[2], lty = 2, lwd = 2)
lines(tracking_ts_trackingonly$dates, tracking_ts_trackingonly$prop, col = cols[5], lty = 1, lwd = 2)
# define dates
dates_points_surveyplus_message_treatment <- filter(tracking_ts_surveyplus_message_treatment, dates %in% as.Date(c(fieldtime_w1, fieldtime_w2, fieldtime_w3)))
dates_points_surveyplus_message_control <- filter(tracking_ts_surveyplus_message_control, dates %in% as.Date(c(fieldtime_w1, fieldtime_w2, fieldtime_w3)))
dates_points_trackingonly <- filter(tracking_ts_trackingonly, dates %in% as.Date(c(fieldtime_w1, fieldtime_w2, fieldtime_w3)))
# plot points
points(dates_points_surveyplus_message_treatment$dates, dates_points_surveyplus_message_treatment$prop, pch = 21, col = "black", bg = cols[1])
points(dates_points_surveyplus_message_control$dates, dates_points_surveyplus_message_control$prop, pch = 21, col = "black", bg = cols[2])
points(dates_points_trackingonly$dates, dates_points_trackingonly$prop, pch = 21, col = "black", bg = cols[5])
# plot text labels
text(dates_points_surveyplus_message_treatment$dates, dates_points_surveyplus_message_treatment$prop, str_replace(sprintf("%0.2f", round(dates_points_surveyplus_message_treatment$prop, 2)), "^0", ""), pos = 3, cex = .7, col = cols[1])
text(dates_points_surveyplus_message_control$dates, dates_points_surveyplus_message_control$prop, str_replace(sprintf("%0.2f", round(dates_points_surveyplus_message_control$prop, 2)), "^0", ""), pos = 1, cex = .7, col = cols[2])
text(dates_points_trackingonly$dates, dates_points_trackingonly$prop, str_replace(sprintf("%0.2f", round(dates_points_trackingonly$prop, 2)), "^0", ""), col = cols[5], pos = 1, cex = .7)
# add axis labels
axis(2, seq(0, .7, .1), seq(0, .7, .1), las = 1, cex.axis = .7)
# add legend
legend("topleft", 
       y.intersp = 1.5,
       legend = c(paste0("Survey-tracking, treatment", " (n = ", tracking_ts_surveyplus_message_treatment$nobs[1], ")"), 
                  paste0("Survey-tracking, control", " (n = ", tracking_ts_surveyplus_message_control$nobs[1], ")"),
                  paste0("Tracking-only", " (n = ", tracking_ts_trackingonly$nobs[1], ")")
       ), 
       col = cols[c(1,2,5)], 
       lty = c(1, 2, 1), 
       lwd = 2,
       bty = "n", 
       pt.cex = 2, 
       cex = .7, 
       text.col = cols[c(1,2,5)], 
       horiz = F)
# title label
par(xpd=TRUE)
rect(ybottom = .7, ytop = .77, xleft = range(tracking_ts_surveyplus$dates)[1], xright = range(tracking_ts_surveyplus$dates)[2], col = "lightgrey", border = "black")
par(xpd=FALSE)
axis(3, at = tracking_ts_surveyplus$dates[round(length(tracking_ts_surveyplus$dates)/2)], label = "Message experiment groups", line = -.8, tick = FALSE)

# INCENTIVE TREATMENT
par(mar=c(2.5,1,2,1))
# baseline TS
plot(tracking_ts_surveyplus_incent_treatment$dates, tracking_ts_surveyplus_incent_treatment$prop, type = "l", xaxt = "n", yaxt = "n", xaxs="i", yaxs="i", ylim = c(0, .7), xlim = range(tracking_ts_surveyplus$dates), xlab = NA, ylab = "")
# waves
rect(xleft = as.Date(fieldtime_w1[1]), xright = as.Date(fieldtime_w1[2]), ybottom = 0, ytop = 1, col = rgb(0.5,0.5,0.5, 1/4), border = NA)
rect(xleft = as.Date(fieldtime_w2[1]), xright = as.Date(fieldtime_w2[2]), ybottom = 0, ytop = 1, col = rgb(0.5,0.5,0.5, 1/4), border = NA)
rect(xleft = as.Date(fieldtime_w3[1]), xright = as.Date(fieldtime_w3[2]), ybottom = 0, ytop = 1, col = rgb(0.5,0.5,0.5, 1/4), border = NA)
text(mean(c(as.Date(fieldtime_w1[1]), as.Date(fieldtime_w1[2]))), .63, "Wave 1\n(Message\ntreatment)", cex = .7)
text(mean(c(as.Date(fieldtime_w2[1]), as.Date(fieldtime_w2[2]))), .63, "Wave 2\n(Incentive\ntreatment)", cex = .7)
text(mean(c(as.Date(fieldtime_w3[1]), as.Date(fieldtime_w3[2]))), .65, "Wave 3", cex = .7)
# add grid
datelabels <-  tracking_ts_surveyplus$dates[seq(1, length(tracking_ts_surveyplus$dates), 7)]
abline(h = seq(0, .7, .1), col = "grey80", lty = 3)
abline(v = datelabels, col = "grey80", lty = 3)
# add CIs as polygons
# incent treatment
CI.x <- c(tracking_ts_surveyplus_incent_treatment$dates, rev(tracking_ts_surveyplus_incent_treatment$dates)) # polygons are drawn clockwise
CI.y <- c(tracking_ts_surveyplus_incent_treatment$prop_cihi, rev(tracking_ts_surveyplus_incent_treatment$prop_cilo))
CI.col <- adjustcolor(cols[3],alpha.f=0.25)
polygon(CI.x, CI.y, col=CI.col, border=NA)
# incent control
CI.x <- c(tracking_ts_surveyplus_incent_control$dates, rev(tracking_ts_surveyplus_incent_control$dates)) # polygons are drawn clockwise
CI.y <- c(tracking_ts_surveyplus_incent_control$prop_cihi, rev(tracking_ts_surveyplus_incent_control$prop_cilo))
CI.col <- adjustcolor(cols[4],alpha.f=0.25)
polygon(CI.x, CI.y, col=CI.col, border=NA)
# add main TS
lines(tracking_ts_surveyplus_incent_treatment$dates, tracking_ts_surveyplus_incent_treatment$prop, col = cols[3], lty = 1, lwd = 2)
lines(tracking_ts_surveyplus_incent_control$dates, tracking_ts_surveyplus_incent_control$prop, col = cols[4], lty = 2, lwd = 2)
# define dates
dates_points_surveyplus_incent_treatment <- filter(tracking_ts_surveyplus_incent_treatment, dates %in% as.Date(c(fieldtime_w1, fieldtime_w2, fieldtime_w3)))
dates_points_surveyplus_incent_control <- filter(tracking_ts_surveyplus_incent_control, dates %in% as.Date(c(fieldtime_w1, fieldtime_w2, fieldtime_w3)))
# plot points
points(dates_points_surveyplus_incent_treatment$dates, dates_points_surveyplus_incent_treatment$prop, pch = 21, col = "black", bg = cols[3])
points(dates_points_surveyplus_incent_control$dates, dates_points_surveyplus_incent_control$prop, pch = 21, col = "black", bg = cols[4])
# plot text labels
text(dates_points_surveyplus_incent_treatment$dates, dates_points_surveyplus_incent_treatment$prop, str_replace(sprintf("%0.2f", round(dates_points_surveyplus_incent_treatment$prop, 2)), "^0", ""), pos = 1, cex = .7, col = cols[3])
text(dates_points_surveyplus_incent_control$dates, dates_points_surveyplus_incent_control$prop, str_replace(sprintf("%0.2f", round(dates_points_surveyplus_incent_control$prop, 2)), "^0", ""), pos = 3, cex = .7, col = cols[4])
# add axis labels
axis(1, datelabels, format(datelabels, "%b %d"), cex.axis = .7)
axis(2, seq(0, .7, .1), seq(0, .7, .1), las = 1, cex.axis = .7)
# add legend
legend("topleft", 
       y.intersp = 1.5,
       legend = c(paste0("Survey-tracking, treatment", " (n = ", tracking_ts_surveyplus_incent_treatment$nobs[1], ")"),
                  paste0("Survey-tracking, control", " (n = ", tracking_ts_surveyplus_incent_control$nobs[1], ")")
       ), 
       col = cols[3:5], 
       lty = c(1, 2,1), 
       lwd = 2,
       bty = "n", 
       pt.cex = 2, 
       cex = .7, 
       text.col = cols[3:5], 
       horiz = F)
# overall axis label
mtext("App adoption rate", 2, 2, outer=TRUE, las=0)
# title label
par(xpd=TRUE)
rect(ybottom = .7, ytop = .77, xleft = range(tracking_ts_surveyplus$dates)[1], xright = range(tracking_ts_surveyplus$dates)[2], col = "lightgrey", border = "black")
par(xpd=FALSE)
axis(3, at = tracking_ts_surveyplus$dates[round(length(tracking_ts_surveyplus$dates)/2)], label = "Incentive experiment groups", line = -.8, tick = FALSE)
dev.off()




# plot by device manufacturer, full tracking period
pdf(file="../figures/app-usage-tracking-timeseries-apple.pdf", height = 5, width = 9, family="Helvetica")
par(oma=c(0,0,0,0))
par(mar=c(2.5,2,1,1))
cols4 <- c("#b2182b","#e08214","#2166ac","#4393c3")
# baseline TS
plot(tracking_ts_surveyplus_apple$dates, tracking_ts_surveyplus_apple$prop, type = "l", xaxt = "n", yaxt = "n", xaxs="i", yaxs="i", ylim = c(0, .6), xlim = range(tracking_ts_surveyplus_apple$dates), xlab = NA, ylab = "App adoption rate")
# add grid
datelabels <-  tracking_ts_surveyplus_apple$dates[seq(1, length(tracking_ts_surveyplus_apple$dates), 7)]
abline(h = seq(0, .6, .1), col = "grey80", lty = 3)
abline(v = datelabels, col = "grey80", lty = 3)
# label waves
rect(xleft = as.Date(fieldtime_w1[1]), xright = as.Date(fieldtime_w1[2]), ybottom = 0, ytop = 1, col = rgb(0.5,0.5,0.5, 1/4), border = NA)
rect(xleft = as.Date(fieldtime_w2[1]), xright = as.Date(fieldtime_w2[2]), ybottom = 0, ytop = 1, col = rgb(0.5,0.5,0.5, 1/4), border = NA)
rect(xleft = as.Date(fieldtime_w3[1]), xright = as.Date(fieldtime_w3[2]), ybottom = 0, ytop = 1, col = rgb(0.5,0.5,0.5, 1/4), border = NA)
text(mean(c(as.Date(fieldtime_w1[1]), as.Date(fieldtime_w1[2]))), .56, "Wave 1\n(Message\nTreatment)", cex = .7)
text(mean(c(as.Date(fieldtime_w2[1]), as.Date(fieldtime_w2[2]))), .56, "Wave 2\n(Incentive\nTreatment)", cex = .7)
text(mean(c(as.Date(fieldtime_w3[1]), as.Date(fieldtime_w3[2]))), .58, "Wave 3", cex = .7)
# add CIs as polygons
# apple, survey
CI.x <- c(tracking_ts_surveyplus_apple$dates, rev(tracking_ts_surveyplus_apple$dates)) # polygons are drawn clockwise
CI.y <- c(tracking_ts_surveyplus_apple$prop_cihi, rev(tracking_ts_surveyplus_apple$prop_cilo))
CI.col <- adjustcolor(cols4[1],alpha.f=0.25)
polygon(CI.x, CI.y, col=CI.col, border=NA)
# no apple, survey
CI.x <- c(tracking_ts_surveyplus_noapple$dates, rev(tracking_ts_surveyplus_noapple$dates)) # polygons are drawn clockwise
CI.y <- c(tracking_ts_surveyplus_noapple$prop_cihi, rev(tracking_ts_surveyplus_noapple$prop_cilo))
CI.col <- adjustcolor(cols4[2],alpha.f=0.25)
polygon(CI.x, CI.y, col=CI.col, border=NA)
# apple, no survey
CI.x <- c(tracking_ts_trackingonly_apple$dates, rev(tracking_ts_trackingonly_apple$dates)) # polygons are drawn clockwise
CI.y <- c(tracking_ts_trackingonly_apple$prop_cihi, rev(tracking_ts_trackingonly_apple$prop_cilo))
CI.col <- adjustcolor(cols4[3],alpha.f=0.25)
polygon(CI.x, CI.y, col=CI.col, border=NA)
# no apple, no survey
CI.x <- c(tracking_ts_trackingonly_noapple$dates, rev(tracking_ts_trackingonly_noapple$dates)) # polygons are drawn clockwise
CI.y <- c(tracking_ts_trackingonly_noapple$prop_cihi, rev(tracking_ts_trackingonly_noapple$prop_cilo))
CI.col <- adjustcolor(cols4[4],alpha.f=0.25)
polygon(CI.x, CI.y, col=CI.col, border=NA)
# add main TS
lines(tracking_ts_surveyplus_apple$dates, tracking_ts_surveyplus_apple$prop, col = cols4[1], lty = 1, lwd = 2)
lines(tracking_ts_surveyplus_noapple$dates, tracking_ts_surveyplus_noapple$prop, col = cols4[2], lty = 2, lwd = 2)
lines(tracking_ts_trackingonly_apple$dates, tracking_ts_trackingonly_apple$prop, col = cols4[3], lty = 1, lwd = 2)
lines(tracking_ts_trackingonly_noapple$dates, tracking_ts_trackingonly_noapple$prop, col = cols4[4], lty = 2, lwd = 2)
# define dates
dates_points_surveyplus_apple <- filter(tracking_ts_surveyplus_apple, dates %in% as.Date(c(fieldtime_w1, fieldtime_w2, fieldtime_w3)))
dates_points_surveyplus_noapple <- filter(tracking_ts_surveyplus_noapple, dates %in% as.Date(c(fieldtime_w1, fieldtime_w2, fieldtime_w3)))
dates_points_trackingonly_apple <- filter(tracking_ts_trackingonly_apple, dates %in% as.Date(c(fieldtime_w1, fieldtime_w2, fieldtime_w3)))
dates_points_trackingonly_noapple <- filter(tracking_ts_trackingonly_noapple, dates %in% as.Date(c(fieldtime_w1, fieldtime_w2, fieldtime_w3)))
# plot points
points(dates_points_surveyplus_apple$dates, dates_points_surveyplus_apple$prop, pch = 21, col = "black", bg = cols4[1])
points(dates_points_surveyplus_noapple$dates, dates_points_surveyplus_noapple$prop, pch = 21, col = "black", bg = cols4[2])
points(dates_points_trackingonly_apple$dates, dates_points_trackingonly_apple$prop, pch = 21, col = "black", bg = cols4[3])
points(dates_points_trackingonly_noapple$dates, dates_points_trackingonly_noapple$prop, pch = 21, col = "black", bg = cols4[4])
# plot text labels
text(dates_points_surveyplus_apple$dates, dates_points_surveyplus_apple$prop, str_replace(sprintf("%0.2f", round(dates_points_surveyplus_apple$prop, 2)), "^0", ""), pos = 3, cex = .7, col = cols4[1])
text(dates_points_surveyplus_noapple$dates, dates_points_surveyplus_noapple$prop, str_replace(sprintf("%0.2f", round(dates_points_surveyplus_noapple$prop, 2)), "^0", ""), pos = 1, cex = .7, col = cols4[2])
text(dates_points_trackingonly_apple$dates, dates_points_trackingonly_apple$prop, str_replace(sprintf("%0.2f", round(dates_points_trackingonly_apple$prop, 2)), "^0", ""), pos = 1, cex = .7, col = cols4[3])
text(dates_points_trackingonly_noapple$dates, dates_points_trackingonly_noapple$prop, str_replace(sprintf("%0.2f", round(dates_points_trackingonly_noapple$prop, 2)), "^0", ""), pos = 3, cex = .7, col = cols4[4])
# add axis labels
axis(1, datelabels, format(datelabels, "%b %d"), cex.axis = .7)
axis(2, seq(0, .6, .1), seq(0, .6, .1), las = 1, cex.axis = .7)
# add legend
legend("topleft", 
       y.intersp = 1.5,
       legend = c(paste0("Survey-tracking, Apple device", " (n = ", tracking_ts_surveyplus_apple$nobs[1], ")"), 
                  paste0("Survey-tracking, Android device", " (n = ", tracking_ts_surveyplus_noapple$nobs[1], ")"),
                  paste0("Tracking-only, Apple device", " (n = ", tracking_ts_trackingonly_apple$nobs[1], ")"),
                  paste0("Tracking-only, Android device", " (n = ", tracking_ts_trackingonly_noapple$nobs[1], ")")
       ), 
       col = cols4[1:4], 
       lty = c(1, 2, 1, 2), 
       lwd = 2,
       bty = "n", 
       pt.cex = 2, 
       cex = .7, 
       text.col = cols4, 
       horiz = F)
dev.off()




# tracking data descriptives -------------------------

# number of total instances of app interactions
nrow(tracking_df)

# how many respondents delivering tracking data
nrow(tracking_df_unique)

# how many respondents with app usage activity
table(tracking_df_unique$track_installed)

# how many seconds interaction on average?
summary(tracking_df_vars$track_usage_time_avg)

# tracked on how many devices?
table(tracking_df_vars$track_n_devices)

# adoption rate by OS
table(tracking_df_unique$device_os, tracking_df_unique$track_installed)
table(tracking_df_unique$device_manufacturer, tracking_df_unique$track_installed)

descr::CrossTable(tracking_df_unique$device_os, tracking_df_unique$track_installed,
                             prop.c = TRUE, prop.t = FALSE, prop.chisq = FALSE,
                             dnn = c("Operating System", "Tracked App usage"),
                             digits = 2)




# quota table (sample) -------------------------

ger_w1_quotas_df <- read_excel("../data/ger-w1-quotas.xlsx")

ger_survey_quotas_df <- transmute(ger_df_wide,
                                  sample_type.1, 
                                  male = (gender.1 == 1),
                                 female = (gender.1 == 2),
                                 age_18_29 = (age_cat.1 == "18-29"),
                                 age_30_39 = (age_cat.1 == "30-39"),
                                  age_40_49 = (age_cat.1 == "40-49"),
                                  age_50_59 = (age_cat.1 == "50-59"),
                                  age_60_80 = (age_cat.1 == "60-80"),
                                edu_lo = (educ_cat.1 == "lo"),
                                 edu_mid = (educ_cat.1 == "mid"),
                                 edu_hi = (educ_cat.1 == "hi")
)

# compute sample statistics
rounded_mean <- function(x) round(mean(x, na.rm=TRUE)*100, 1)

ger_survey_quotas_df_pooled <- ger_survey_quotas_df %>% summarise_all(list(rounded_mean)) %>% pivot_longer(-sample_type.1, names_to = "variable", values_to = "value") %>% dplyr::select(-sample_type.1) %>% rename(stat_pooled = value)

ger_survey_quotas_df_sum <- ger_survey_quotas_df %>% group_by(sample_type.1) %>%
  summarise_all(list(rounded_mean)) %>% pivot_longer(-sample_type.1, names_to = "variable", values_to = "value") %>% filter(!is.na(sample_type.1))
ger_survey_quotas_df_surveyonly <- filter(ger_survey_quotas_df_sum, sample_type.1 == "surveyonly") %>% rename(stat_surveyonly = value) %>% dplyr::select(-sample_type.1)
ger_survey_quotas_df_surveytracking <- filter(ger_survey_quotas_df_sum, sample_type.1 == "surveytracking") %>% rename(stat_surveytracking = value) %>% dplyr::select(-sample_type.1)

# combine tables
quotas_compare_df <- ger_w1_quotas_df %>% 
  left_join(ger_survey_quotas_df_pooled, by = "variable") %>% 
  left_join(ger_survey_quotas_df_surveyonly, by = "variable") %>% 
  left_join(ger_survey_quotas_df_surveytracking, by = "variable") %>%
  dplyr::select(label2, quota, stat_pooled, stat_surveyonly, stat_surveytracking)

# prepare for export
quotas_compare_table <- rbind(data.frame(label2 = "Gender", quota = NA, stat_pooled = NA, stat_surveyonly = NA, stat_surveytracking = NA, stringsAsFactors = FALSE),
                              quotas_compare_df[1:2,],
                              data.frame(label2 = "Age", quota = NA, stat_pooled = NA, stat_surveyonly = NA, stat_surveytracking = NA, stringsAsFactors = FALSE),
                              quotas_compare_df[3:7,],
                              data.frame(label2 = "Education", quota = NA, stat_pooled = NA, stat_surveyonly = NA, stat_surveytracking = NA, stringsAsFactors = FALSE),
                              quotas_compare_df[8:10,])

# export
addtorow <- list()
addtorow$pos <- list(0)
addtorow$command <- paste0(paste0('& & \\multicolumn{3}{c}{Sample}  ', collapse=''), '\\\\',paste0('\\cmidrule(r){3-5} Respondent characteristic & b4p \'19 & Pooled & Survey-only  & Survey-tracking', collapse=''), '\\\\')
quotas_compare_table_xtab <- xtable(quotas_compare_table, digits = 0, label = "tab:quotabalance")
caption(quotas_compare_table_xtab) <- "Descriptive statistics of respondent characteristics (quotas), by sample"
print(quotas_compare_table_xtab, type = "latex", sanitize.text.function = function(x){x}, size = "small", table.placement = "h!", add.to.row=addtorow, include.rownames = FALSE, include.colnames = FALSE, caption.placement = "top", file = "../figures/quota_balance_table.tex")



# message experiment balance tables (predictors incl. pre-treatment outcomes; treatments vs. control) --------------

# compute covariate means by treatment group
covar_means_df <- ger_df_wide[,c("treat.1", covars_adapted_df$variable)] %>% group_by(treat.1) %>% summarise_all(list(~ mean(.x, na.rm = TRUE))) %>% filter(!is.na(treat.1))
prosoc_means_df <- filter(covar_means_df, treat.1 == "prosocial") %>% pivot_longer(-treat.1, names_to = "variable", values_to = "prosoc_mean") %>% dplyr::select(-treat.1)
selfint_means_df <- filter(covar_means_df, treat.1 == "selfinterest") %>% pivot_longer(-treat.1, names_to = "variable", values_to = "selfint_mean") %>% dplyr::select(-treat.1)
control_means_df <- filter(covar_means_df, treat.1 == "control") %>% pivot_longer(-treat.1, names_to = "variable", values_to = "control_mean") %>% dplyr::select(-treat.1)

# combine into tables, compute difference + p-values
prosoc_control_means_df <- left_join(prosoc_means_df, control_means_df, by = "variable")
prosoc_control_means_df$variable_label <- str_replace(covars_adapted_df$var_label, "\\n", " ")
prosoc_control_means_df$difference_t_c = prosoc_control_means_df$prosoc_mean - prosoc_control_means_df$control_mean
prosoc_control_means_df$p_value <- map_dbl(covars_adapted_df$variable, t_test_out, "treat_prosoc.1", filter(ger_df_wide, treat_selfint.1 != TRUE))
prosoc_control_means_df <- dplyr::select(prosoc_control_means_df, variable_label, prosoc_mean, control_mean, difference_t_c, p_value)
names(prosoc_control_means_df) <- c("Covariate", "Treatment", "Control", "Diff. (T-C)", "p value")

selfint_control_means_df <- left_join(selfint_means_df, control_means_df, by = "variable")
selfint_control_means_df$variable_label <- str_replace(covars_adapted_df$var_label, "\\n", " ")
selfint_control_means_df$difference_t_c = selfint_control_means_df$selfint_mean - selfint_control_means_df$control_mean
selfint_control_means_df$p_value <- map_dbl(covars_adapted_df$variable, t_test_out, "treat_selfint.1", filter(ger_df_wide, treat_prosoc.1 != TRUE))
selfint_control_means_df <- dplyr::select(selfint_control_means_df, variable_label, selfint_mean, control_mean, difference_t_c, p_value)
names(selfint_control_means_df) <- c("Covariate", "Treatment", "Control", "Diff. (T-C)", "p value")

# export tables
covar_balance_prosoc_controlxtab <- xtable(prosoc_control_means_df, digits = 2, label = "tab:prosoc-balance")
caption(covar_balance_prosoc_controlxtab) <- "Covariate balance: Pro-social message treatment vs. Control (means reported)"
print(covar_balance_prosoc_controlxtab, type = "latex", sanitize.text.function = function(x){x}, size = "footnotesize", table.placement = "h!", include.rownames = FALSE, include.colnames = TRUE, caption.placement = "top", file = "../figures/prosoc_balance_table.tex")

covar_balance_selfint_controlxtab <- xtable(selfint_control_means_df, digits = 2, label = "tab:selfint-balance")
caption(covar_balance_selfint_controlxtab) <- "Covariate balance: Self-interest message treatment vs. Control (means reported)"
print(covar_balance_selfint_controlxtab, type = "latex", sanitize.text.function = function(x){x}, size = "footnotesize", table.placement = "h!", include.rownames = FALSE, include.colnames = TRUE, caption.placement = "top", file = "../figures/selfint_balance_table.tex")



# incentivization experiment balance tables (predictors incl. pre-treatment outcomes; treatments vs. control) --------------

# compute covariate means by treatment group
covar_means_df <- ger_df_wide[,c("treat_incent.2", covars_adapted_df$variable)] %>% group_by(treat_incent.2) %>% summarise_all(list(~ mean(.x, na.rm = TRUE))) %>% filter(!is.na(treat_incent.2))
eur1_means_df <- filter(covar_means_df, treat_incent.2 == "EUR 1") %>% pivot_longer(-treat_incent.2, names_to = "variable", values_to = "eur1_mean") %>% dplyr::select(-treat_incent.2)
eur2_means_df <- filter(covar_means_df, treat_incent.2 == "EUR 2") %>% pivot_longer(-treat_incent.2, names_to = "variable", values_to = "eur2_mean") %>% dplyr::select(-treat_incent.2)
eur5_means_df <- filter(covar_means_df, treat_incent.2 == "EUR 5") %>% pivot_longer(-treat_incent.2, names_to = "variable", values_to = "eur5_mean") %>% dplyr::select(-treat_incent.2)
control_means_df <- filter(covar_means_df, treat_incent.2 == "Control") %>% pivot_longer(-treat_incent.2, names_to = "variable", values_to = "control_mean") %>% dplyr::select(-treat_incent.2)

# combine into tables, compute difference + p-values
eur1_control_means_df <- left_join(eur1_means_df, control_means_df, by = "variable")
eur1_control_means_df$variable_label <- str_replace(covars_adapted_df$var_label, "\\n", " ")
eur1_control_means_df$difference_t_c = eur1_control_means_df$eur1_mean - eur1_control_means_df$control_mean
eur1_control_means_df$p_value <- map_dbl(covars_adapted_df$variable, t_test_out, "treat_incent_eur1.2", filter(ger_df_wide, !(treat_incent.2 %in% c("EUR 2", "EUR 5"))))
eur1_control_means_df <- dplyr::select(eur1_control_means_df, variable_label, eur1_mean, control_mean, difference_t_c, p_value)
names(eur1_control_means_df) <- c("Covariate", "EUR 1", "Control", "Diff. (T-C)", "p value")

eur2_control_means_df <- left_join(eur2_means_df, control_means_df, by = "variable")
eur2_control_means_df$variable_label <- str_replace(covars_adapted_df$var_label, "\\n", " ")
eur2_control_means_df$difference_t_c = eur2_control_means_df$eur2_mean - eur2_control_means_df$control_mean
eur2_control_means_df$p_value <- map_dbl(covars_adapted_df$variable, t_test_out, "treat_incent_eur2.2", filter(ger_df_wide, !(treat_incent.2 %in% c("EUR 1", "EUR 5"))))
eur2_control_means_df <- dplyr::select(eur2_control_means_df, variable_label, eur2_mean, control_mean, difference_t_c, p_value)
names(eur2_control_means_df) <- c("Covariate", "EUR 2", "Control", "Diff. (T-C)", "p value")

eur5_control_means_df <- left_join(eur5_means_df, control_means_df, by = "variable")
eur5_control_means_df$variable_label <- str_replace(covars_adapted_df$var_label, "\\n", " ")
eur5_control_means_df$difference_t_c = eur5_control_means_df$eur5_mean - eur5_control_means_df$control_mean
eur5_control_means_df$p_value <- map_dbl(covars_adapted_df$variable, t_test_out, "treat_incent_eur5.2", filter(ger_df_wide, !(treat_incent.2 %in% c("EUR 1", "EUR 2"))))
eur5_control_means_df <- dplyr::select(eur5_control_means_df, variable_label, eur5_mean, control_mean, difference_t_c, p_value)
names(eur5_control_means_df) <- c("Covariate", "EUR 5", "Control", "Diff. (T-C)", "p value")

# export tables
covar_balance_eur1_controlxtab <- xtable(eur1_control_means_df, digits = 2, label = "tab:eur1-balance")
caption(covar_balance_eur1_controlxtab) <- "Covariate balance: EUR 1 incentivization treatment vs. Control (means reported)"
print(covar_balance_eur1_controlxtab, type = "latex", sanitize.text.function = function(x){x}, size = "footnotesize", table.placement = "h!", include.rownames = FALSE, include.colnames = TRUE, caption.placement = "top", file = "../figures/eur1_balance_table.tex")

covar_balance_eur2_controlxtab <- xtable(eur2_control_means_df, digits = 2, label = "tab:eur2-balance")
caption(covar_balance_eur2_controlxtab) <- "Covariate balance: EUR 2 incentivization treatment vs. Control (means reported)"
print(covar_balance_eur2_controlxtab, type = "latex", sanitize.text.function = function(x){x}, size = "footnotesize", table.placement = "h!", include.rownames = FALSE, include.colnames = TRUE, caption.placement = "top", file = "../figures/eur2_balance_table.tex")

covar_balance_eur5_controlxtab <- xtable(eur5_control_means_df, digits = 2, label = "tab:eur5-balance")
caption(covar_balance_eur5_controlxtab) <- "Covariate balance: EUR 5 incentivization treatment vs. Control (means reported)"
print(covar_balance_eur5_controlxtab, type = "latex", sanitize.text.function = function(x){x}, size = "footnotesize", table.placement = "h!", include.rownames = FALSE, include.colnames = TRUE, caption.placement = "top", file = "../figures/eur5_balance_table.tex")




# compliance statistics --------------

# time spent on treatment
ger_w1_time_df %>% group_by(treatment) %>% summarize(median_time_intervention = median(t_app_intervention_Page_Submit, na.rm = TRUE))
p <- ger_w1_time_df %>%
  ggplot( aes(x=t_app_intervention_Page_Submit, fill=treatment)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  labs(fill="")
p
quantile(ger_w1_time_df$t_app_intervention_Page_Submit[ger_w1_time_df$treatment != 3], seq(0, 1, .05), na.rm = TRUE)

# how many complied
crosstab(ger_df_wide, row.vars = "treat_comp.1", col.vars = "treat.1", type = c("c"), style = "long", addmargins = FALSE)



# manipulation check statistics -------------------------

label(ger_w1_df$app_knowledge_1)
crosstab(ger_w1_df, row.vars = "app_knowledge_1", col.vars = "treatment", type = c("c"), style = "long", addmargins = FALSE)

label(ger_w1_df$app_knowledge_2)
crosstab(ger_w1_df, row.vars = "app_knowledge_2", col.vars = "treatment", type = c("c"), style = "long", addmargins = FALSE)

label(ger_w1_df$app_knowledge_3)
crosstab(ger_w1_df, row.vars = "app_knowledge_3", col.vars = "treatment", type = c("c"), style = "long", addmargins = FALSE)

label(ger_w1_df$app_knowledge_4)
crosstab(ger_w1_df, row.vars = "app_knowledge_4", col.vars = "treatment", type = c("c"), style = "long", addmargins = FALSE)

label(ger_w1_df$app_knowledge_5)
crosstab(ger_w1_df, row.vars = "app_knowledge_5", col.vars = "treatment", type = c("c"), style = "long", addmargins = FALSE)

# compute correct shares by treatment status
know_cor_df <- dplyr::select(ger_df_wide, treat.1, ends_with("_cor.1") & !contains("icon") & !contains("screen")) %>% 
  group_by(treat.1) %>% 
  summarize_all(mean, na.rm = TRUE) %>% 
  pivot_longer(-treat.1, names_to = "variable") %>%
  filter(!is.na(treat.1))


var_labels <- c(
  "(3) Data collected by\n the app are stored\ncentrally at RKI (F)", 
  " (2) The more app users,\nthe better vulnerable\npeople are\nprotected (T)",
  "(1) Despite anonymity,\nthe app can help me\nknow faster about\npossible infection (T)",
  "(5) To date the app\nhas been installed\nabout 10 million times (F)",
  "(4) If many people use\n the app, return to daily\nroutine possible almost\nwithout restrictions (T)"
  )
var_order <- c(3, 2, 1, 5, 4)
know_cor_df$var_labels <- factor(rep(var_labels, 3), levels = var_labels[var_order])
know_cor_df$Treatment <- recode_factor(know_cor_df$treat.1, `control` = "Control", `prosocial` = "Pro-social", `selfinterest` = "Self-interest")

# compute max value across treatments
know_cor_df %<>% group_by(variable) %>% mutate(max_prop = max(value)) %>% ungroup()

# compute t tests
know_items_cor <- sprintf("app_knowledge_%d_cor.1", 1:5)
t_test_prosoc_control <- map_dbl(know_items_cor, t_test_out, "treat_prosoc.1", filter(ger_df_wide, treat_selfint.1 != TRUE))
t_test_selfint_control <- map_dbl(know_items_cor, t_test_out, "treat_selfint.1", filter(ger_df_wide, treat_prosoc.1 != TRUE))
t_test_prosoc_selfint <- map_dbl(know_items_cor, t_test_out, "treat_selfint.1", filter(ger_df_wide, treat.1 != "control"))

# set up segments
segment_data_control_prosoc = data.frame(
  x = .7 + (0:4),
  xend = 1:5,
  y = know_cor_df$max_prop[var_order] + .02,
  yend = know_cor_df$max_prop[var_order] + .02
)
segment_data_prosoc_selfint = data.frame(
  x = 1 + (0:4),
  xend = .3 + 1:5,
  y = know_cor_df$max_prop[var_order] + .08,
  yend = know_cor_df$max_prop[var_order] + .08
)
segment_data_control_selfint = data.frame(
  x = .7 + (0:4),
  xend = .3  + 1:5,
  y = know_cor_df$max_prop[var_order] + .16,
  yend = know_cor_df$max_prop[var_order] + .16
)

# set up annotations
annot_prosoc_control <- paste0("p = ", sprintf("%0.2f", round(t_test_prosoc_control[var_order], 2)))
annot_selfint_control <- paste0("p = ", sprintf("%0.2f", round(t_test_selfint_control[var_order], 2)))
annot_prosoc_selfint <- paste0("p = ", sprintf("%0.2f", round(t_test_prosoc_selfint[var_order], 2)))


# plot
treat_col <- c("black", "blue", "black", "red", "black")
pdf(file="../figures/message-manipcheck-barchart.pdf", height = 5, width = 9)
ggplot(data = know_cor_df, aes(x = var_labels, y = value, fill = Treatment)) +
  geom_bar(stat = "identity", position = position_dodge())  + 
  geom_segment(data = segment_data_control_prosoc, aes(x = x, y = y, xend = xend, yend = yend), inherit.aes = FALSE) + 
  geom_segment(data = segment_data_prosoc_selfint, aes(x = x, y = y, xend = xend, yend = yend), inherit.aes = FALSE) + 
  geom_segment(data = segment_data_control_selfint, aes(x = x, y = y, xend = xend, yend = yend), inherit.aes = FALSE) + 
  annotate("text", x = 0.85 + 0:4, y = know_cor_df$max_prop[var_order] + .05, label = annot_prosoc_control, size = 3) + 
  annotate("text", x = 1 + 0:4, y = know_cor_df$max_prop[var_order] + .19, label = annot_selfint_control, size = 3) + 
  annotate("text", x = 1.15 + 0:4, y = know_cor_df$max_prop[var_order] + .11, label = annot_prosoc_selfint, size = 3) + 
scale_fill_manual(values=c("darkgrey", "blue","red")) +
  xlab("") + ylab("Proportion of correct answers") +
  ylim(0, 1) + 
  ggtitle("") + 
  theme(legend.position="bottom",
        axis.text.x = element_text(colour = treat_col),
        plot.margin = unit(c(0,0.2,0,0), "cm")) #t,r,b,l
dev.off()




# incentivization agreement barchart -------------------------

crosstab(ger_df_wide, row.vars = "treat_incent_agree.2", col.vars = "treat_incent.2", type = c("c"), style = "long", addmargins = FALSE)

# compute agreement by treatment status
agreement_df <- dplyr::select(ger_df_wide, treat_incent_agree.2, treat_incent.2) %>% 
  group_by(treat_incent.2) %>% 
  summarize_all(mean, na.rm = TRUE) %>% 
  pivot_longer(-treat_incent.2, names_to = "variable") %>%
  dplyr::select(-variable) %>% 
  filter(!is.na(value))


# compute max value across treatments
agreement_df$max_agree <- max(agreement_df$value)
  
# compute t tests
t_test_eur1_eur2 <- map_dbl("treat_incent_agree.2", t_test_out, "treat_incent_eur2.2", filter(ger_df_wide, !(treat_incent.2 %in% c("Control", "EUR 5"))))
t_test_eur1_eur5 <- map_dbl("treat_incent_agree.2", t_test_out, "treat_incent_eur5.2", filter(ger_df_wide, !(treat_incent.2 %in% c("Control", "EUR 2"))))
t_test_eur2_eur5 <- map_dbl("treat_incent_agree.2", t_test_out, "treat_incent_eur5.2", filter(ger_df_wide, !(treat_incent.2 %in% c("Control", "EUR 1"))))

# set up annotations
annot_eur1_eur2 <- paste0("p = ", sprintf("%0.2f", round(t_test_eur1_eur2, 2)))
annot_eur1_eur5 <- paste0("p = ", sprintf("%0.2f", round(t_test_eur1_eur5, 2)))
annot_eur2_eur5 <- paste0("p = ", sprintf("%0.2f", round(t_test_eur2_eur5, 2)))

annot_eur1 <- paste0(sprintf("%0.2f", round(agreement_df$value[agreement_df$treat_incent.2 == "EUR 1"], 2)))
annot_eur2 <- paste0(sprintf("%0.2f", round(agreement_df$value[agreement_df$treat_incent.2 == "EUR 2"], 2)))
annot_eur5 <- paste0(sprintf("%0.2f", round(agreement_df$value[agreement_df$treat_incent.2 == "EUR 5"], 2)))

# set up segments
segment_eur1_eur2 <- data.frame(x = 1, xend = 2, y = .5, yend = .5)
segment_eur1_eur5 <- data.frame(x = 1, xend = 3, y = .62, yend = .62)
segment_eur2_eur5 <- data.frame(x = 2, xend = 3, y = .56, yend = .56)

# plot
pdf(file="../figures/incentivization-agreement-barchart.pdf", height = 3.5, width = 3.5)
ggplot(data = agreement_df, aes(x = treat_incent.2, y = value, fill = treat_incent.2)) +
  geom_bar(stat = "identity")  + 
  geom_segment(data = segment_eur1_eur2, aes(x = x, y = y, xend = xend, yend = yend), inherit.aes = FALSE) + 
  geom_segment(data = segment_eur1_eur5, aes(x = x, y = y, xend = xend, yend = yend), inherit.aes = FALSE) + 
  geom_segment(data = segment_eur2_eur5, aes(x = x, y = y, xend = xend, yend = yend), inherit.aes = FALSE) + 
  annotate("text", x = 1, y = agreement_df$value[agreement_df$treat_incent.2 == "EUR 1"] + .025, label = annot_eur1, size = 3) + 
  annotate("text", x = 2, y = agreement_df$value[agreement_df$treat_incent.2 == "EUR 2"] + .025, label = annot_eur2, size = 3) + 
  annotate("text", x = 3, y = agreement_df$value[agreement_df$treat_incent.2 == "EUR 5"] + .025, label = annot_eur5, size = 3) + 
  annotate("text", x = 1.5, y = .53, label = annot_eur1_eur2, size = 3) + 
  annotate("text", x = 2.5, y = .59, label = annot_eur2_eur5, size = 3) + 
  annotate("text", x = 2, y = .65, label = annot_eur1_eur5, size = 3) + 
  scale_fill_manual(values=c("#99d8c9", "#41ae76", "#005824")) +
  xlab("Incentivization") + ylab("Agreement rate") +
  ylim(0, .66) + 
  ggtitle("") + 
  theme(legend.position="none",
        plot.margin = unit(c(0,0.2,0,0), "cm")) #t,r,b,l
dev.off()



# app uptake after incentivization barchart -------------------------

crosstab(ger_df_wide, row.vars = "app_hyb_installed_at_w3", col.vars = "treat_incent.2", type = c("c"), style = "long", addmargins = FALSE)

# compute app installations by treatment status
install_df <- dplyr::select(ger_df_wide, app_hyb_installed_at_w3, treat_incent.2) %>% 
  group_by(treat_incent.2) %>% 
  summarize_all(mean, na.rm = TRUE) %>% 
  pivot_longer(-treat_incent.2, names_to = "variable") %>%
  dplyr::select(-variable) %>% 
  filter(!is.na(value), !is.na(treat_incent.2))


# compute max value across treatments
install_df$max_install <- max(install_df$value)

# compute t tests
t_test_eur1_eur2 <- map_dbl("app_hyb_installed_at_w3", t_test_out, "treat_incent_eur2.2", filter(ger_df_wide, !(treat_incent.2 %in% c("Control", "EUR 5"))))
t_test_eur1_eur5 <- map_dbl("app_hyb_installed_at_w3", t_test_out, "treat_incent_eur5.2", filter(ger_df_wide, !(treat_incent.2 %in% c("Control", "EUR 2"))))
t_test_eur1_control <- map_dbl("app_hyb_installed_at_w3", t_test_out, "treat_incent_control.2", filter(ger_df_wide, !(treat_incent.2 %in% c("EUR 2", "EUR 5"))))
t_test_eur2_eur5 <- map_dbl("app_hyb_installed_at_w3", t_test_out, "treat_incent_eur5.2", filter(ger_df_wide, !(treat_incent.2 %in% c("Control", "EUR 1"))))
t_test_eur2_control <- map_dbl("app_hyb_installed_at_w3", t_test_out, "treat_incent_control.2", filter(ger_df_wide, !(treat_incent.2 %in% c("EUR 1", "EUR 5"))))
t_test_eur5_control <- map_dbl("app_hyb_installed_at_w3", t_test_out, "treat_incent_control.2", filter(ger_df_wide, !(treat_incent.2 %in% c("EUR 1", "EUR 2"))))

# set up annotations
annot_eur1_eur2 <- paste0("p = ", sprintf("%0.2f", round(t_test_eur1_eur2, 2)))
annot_eur1_eur5 <- paste0("p = ", sprintf("%0.2f", round(t_test_eur1_eur5, 2)))
annot_eur1_control <- paste0("p = ", sprintf("%0.2f", round(t_test_eur1_control, 2)))
annot_eur2_eur5 <- paste0("p = ", sprintf("%0.2f", round(t_test_eur2_eur5, 2)))
annot_eur2_control <- paste0("p = ", sprintf("%0.2f", round(t_test_eur2_control, 2)))
annot_eur5_control <- paste0("p = ", sprintf("%0.2f", round(t_test_eur5_control, 2)))
annot_eur1 <- paste0(sprintf("%0.2f", round(install_df$value[install_df$treat_incent.2 == "EUR 1"], 2)))
annot_eur2 <- paste0(sprintf("%0.2f", round(install_df$value[install_df$treat_incent.2 == "EUR 2"], 2)))
annot_eur5 <- paste0(sprintf("%0.2f", round(install_df$value[install_df$treat_incent.2 == "EUR 5"], 2)))
annot_control <- paste0(sprintf("%0.2f", round(install_df$value[install_df$treat_incent.2 == "Control"], 2)))

# set up segments
segment_eur1_control <- data.frame(x = 1, xend = 2, y = .26, yend = .26)
segment_eur1_eur2 <- data.frame(x = 2, xend = 3, y = .30, yend = .30)
segment_eur2_eur5 <- data.frame(x = 3, xend = 4, y = .34, yend = .34)
segment_eur2_control <- data.frame(x = 1, xend = 3, y = .38, yend = .38)
segment_eur1_eur5 <- data.frame(x = 2, xend = 4, y = .42, yend = .42)
segment_eur5_control <- data.frame(x = 1, xend = 4, y = .46, yend = .46)

# plot
pdf(file="../figures/incentivization-installation-barchart.pdf", height = 3.5, width = 3.5)
ggplot(data = install_df, aes(x = treat_incent.2, y = value, fill = treat_incent.2)) +
  geom_bar(stat = "identity")  + 
  geom_segment(data = segment_eur1_control, aes(x = x, y = y, xend = xend, yend = yend), inherit.aes = FALSE) + 
  geom_segment(data = segment_eur1_eur2, aes(x = x, y = y, xend = xend, yend = yend), inherit.aes = FALSE) + 
  geom_segment(data = segment_eur1_eur5, aes(x = x, y = y, xend = xend, yend = yend), inherit.aes = FALSE) + 
  geom_segment(data = segment_eur2_control, aes(x = x, y = y, xend = xend, yend = yend), inherit.aes = FALSE) + 
  geom_segment(data = segment_eur2_eur5, aes(x = x, y = y, xend = xend, yend = yend), inherit.aes = FALSE) + 
  geom_segment(data = segment_eur5_control, aes(x = x, y = y, xend = xend, yend = yend), inherit.aes = FALSE) + 
  annotate("text", x = 1, y = install_df$value[install_df$treat_incent.2 == "Control"] + .025, label = annot_control, size = 3) + 
  annotate("text", x = 2, y = install_df$value[install_df$treat_incent.2 == "EUR 1"] + .025, label = annot_eur1, size = 3) + 
  annotate("text", x = 3, y = install_df$value[install_df$treat_incent.2 == "EUR 2"] + .025, label = annot_eur2, size = 3) + 
  annotate("text", x = 4, y = install_df$value[install_df$treat_incent.2 == "EUR 5"] + .025, label = annot_eur5, size = 3) + 
  annotate("text", x = 1.5, y = .28, label = annot_eur1_control, size = 3) + 
  annotate("text", x = 2.5, y = .32, label = annot_eur1_eur2, size = 3) + 
  annotate("text", x = 3.5, y = .36, label = annot_eur2_eur5, size = 3) + 
  annotate("text", x = 2, y = .40, label = annot_eur2_control, size = 3) + 
  annotate("text", x = 3, y = .44, label = annot_eur1_eur5, size = 3) + 
  annotate("text", x = 2.5, y = .48, label = annot_eur5_control, size = 3) + 
  scale_fill_manual(values=c("darkgrey", "#99d8c9", "#41ae76", "#005824")) +
  xlab("Incentivization") + ylab("Installation rate at W3\n(hybrid measure)") +
  ylim(0, .48) + 
  ggtitle("") + 
  theme(legend.position="none",
        plot.margin = unit(c(0,0.2,0,0), "cm")) #t,r,b,l
dev.off()



# app uptake after incentivization agreement barchart -------------------------

crosstab(filter(ger_df_wide, treat_incent_agree.2 == TRUE), row.vars = "app_hyb_installed_at_w3", col.vars = "treat_incent.2", type = c("c"), style = "long", addmargins = FALSE)

# compute app installations by treatment status
install_df <- ger_df_wide %>% filter(treat_incent_agree.2 == TRUE) %>% dplyr::select(app_hyb_installed_at_w3, treat_incent.2) %>% 
  group_by(treat_incent.2) %>% 
  summarize_all(mean, na.rm = TRUE) %>% 
  pivot_longer(-treat_incent.2, names_to = "variable") %>%
  dplyr::select(-variable) %>% 
  filter(!is.na(value), !is.na(treat_incent.2))


# compute max value across treatments
install_df$max_install <- max(install_df$value)

# compute t tests
t_test_eur1_eur2 <- map_dbl("app_hyb_installed_at_w3", t_test_out, "treat_incent_eur2.2", filter(ger_df_wide, !(treat_incent.2 %in% c("Control", "EUR 5"))))
t_test_eur1_eur5 <- map_dbl("app_hyb_installed_at_w3", t_test_out, "treat_incent_eur5.2", filter(ger_df_wide, !(treat_incent.2 %in% c("Control", "EUR 2"))))
t_test_eur2_eur5 <- map_dbl("app_hyb_installed_at_w3", t_test_out, "treat_incent_eur5.2", filter(ger_df_wide, !(treat_incent.2 %in% c("Control", "EUR 1"))))

# set up annotations
annot_eur1_eur2 <- paste0("p = ", sprintf("%0.2f", round(t_test_eur1_eur2, 2)))
annot_eur1_eur5 <- paste0("p = ", sprintf("%0.2f", round(t_test_eur1_eur5, 2)))
annot_eur2_eur5 <- paste0("p = ", sprintf("%0.2f", round(t_test_eur2_eur5, 2)))
annot_eur1 <- paste0(sprintf("%0.2f", round(install_df$value[install_df$treat_incent.2 == "EUR 1"], 2)))
annot_eur2 <- paste0(sprintf("%0.2f", round(install_df$value[install_df$treat_incent.2 == "EUR 2"], 2)))
annot_eur5 <- paste0(sprintf("%0.2f", round(install_df$value[install_df$treat_incent.2 == "EUR 5"], 2)))

# set up segments
segment_eur1_eur2 <- data.frame(x = 1, xend = 2, y = .54, yend = .54)
segment_eur2_eur5 <- data.frame(x = 2, xend = 3, y = .62, yend = .62)
segment_eur1_eur5 <- data.frame(x = 1, xend = 3, y = .70, yend = .70)

# plot
pdf(file="../figures/incentivization-installation-agreement-barchart.pdf", height = 3.5, width = 3.5)
ggplot(data = install_df, aes(x = treat_incent.2, y = value, fill = treat_incent.2)) +
  geom_bar(stat = "identity")  + 
  geom_segment(data = segment_eur1_eur2, aes(x = x, y = y, xend = xend, yend = yend), inherit.aes = FALSE) + 
  geom_segment(data = segment_eur1_eur5, aes(x = x, y = y, xend = xend, yend = yend), inherit.aes = FALSE) + 
  geom_segment(data = segment_eur2_eur5, aes(x = x, y = y, xend = xend, yend = yend), inherit.aes = FALSE) + 
  annotate("text", x = 1, y = install_df$value[install_df$treat_incent.2 == "EUR 1"] + .025, label = annot_eur1, size = 3) + 
  annotate("text", x = 2, y = install_df$value[install_df$treat_incent.2 == "EUR 2"] + .025, label = annot_eur2, size = 3) + 
  annotate("text", x = 3, y = install_df$value[install_df$treat_incent.2 == "EUR 5"] + .025, label = annot_eur5, size = 3) + 
  annotate("text", x = 1.5, y = .57, label = annot_eur1_eur2, size = 3) + 
  annotate("text", x = 2.5, y = .65, label = annot_eur2_eur5, size = 3) + 
  annotate("text", x = 2, y = .73, label = annot_eur1_eur5, size = 3) + 
  scale_fill_manual(values=c("#99d8c9", "#41ae76", "#005824")) +
  xlab("Incentivization") + ylab("Installation rate at W3 (hybrid measure)\n conditional on agreement in W2") +
  ylim(0, .73) + 
  ggtitle("") + 
  theme(legend.position="none",
        plot.margin = unit(c(0,0.2,0,0), "cm")) #t,r,b,l
dev.off()

