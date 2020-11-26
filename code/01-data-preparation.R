### -----------------------------
### data preparation
### -----------------------------

source("packages.R")
source("functions.R")
#options(scipen=999)

# note on the raw source data ---------------

# the data that is pre-processed in this script is not made publicly available because it contains personal information that could be used to de-anonymize study subjects. we are documenting the pre-processing to be as transparent as possible about our procedures. If access to the data is needed to replicate or modify the analyses, please consult the corresponding author or the study, Simon Munzert (munzert@hertie-school.org).

# files not publicly available:
  # - 20200908 all devices metadata.csv
  # - 20200923 all devices views de.rki.coronawarnapp.csv
  # - 20200908 socio demographics.csv
  # - Corona_September 9, 2020_10.54.sav
  # - Corona - GER Wave 2_September 9, 2020_10.55.sav
  # - Corona - GER Wave 3_September 24, 2020_08.40.sav
  # - hertie-corona-ger-w1-completes.xlsx
  # - hertie-corona-ger-w2-completes.xlsx
  # - hertie-corona-ger-w3-completes.xlsx
  # - cases_landkreis.csv

# anonymized data publicly available
  # - ger_df_wide.RDS


# import data ------------

# tracking data
devices_df <- read_csv("../data/20200908 all devices metadata.csv")
tracking_df <- read_csv("../data/20200923 all devices views de.rki.coronawarnapp.csv")
tracking_sociodem_df <- read_csv("../data/20200908 socio demographics.csv")
date_data <- "20200921"

# wave 1
dat <- read_spss("../data/Corona_September 9, 2020_10.54.sav") 
ger_w1_full_df <- dplyr::filter(dat, Q_Language == "DE"  & !is.na(t_end_screen_Page_Submit) & psid != "") %>% 
                dplyr::select(-contains("pol"),-contains("_ita"),-contains("ita_"),-contains("bra"),-contains("usa")) 
ger_w1_df <- ger_w1_full_df %>% dplyr::select(-starts_with("t_"), t_app_intervention_Page_Submit)
ger_w1_df$wave <- 1
ger_w1_time_df <- ger_w1_full_df %>% dplyr::select(ResponseId, psid, tic, treatment, sample_type, starts_with("t_"))
dplyr::select(ger_w1_df, psid, sample_type)  %>% write_xlsx(path = "../data/hertie-corona-ger-w1-completes.xlsx")

# psids
tracking_survey_psids <- ger_w1_df$psid[ger_w1_df$sample_type == "surveytracking"]
surveyonly_psids <- ger_w1_df$psid[ger_w1_df$sample_type == "surveyonly"]
tracking_only_psids <- tracking_sociodem_df$psid[!(tracking_sociodem_df$psid %in% tracking_survey_psids)]


# wave 2
dat <- read_spss("../data/Corona - GER Wave 2_September 9, 2020_10.55.sav") 
ger_w2_full_df <- filter(dat, Q_Language == "DE" & !is.na(t_end_screen_Page_Submit) & psid != "")
ger_w2_df <- ger_w2_full_df %>% dplyr::select(-starts_with("t_"), t_app_incentive_Page_Submit)
ger_w2_df$wave <- 2
ger_w2_time_df <- ger_w2_full_df %>% dplyr::select(ResponseId, psid, tic, sample_type, starts_with("t_"))
dplyr::select(ger_w2_df, psid, sample_type)  %>% write_xlsx(path = "../data/hertie-corona-ger-w2-completes.xlsx")

# wave 3
dat <- read_spss("../data/Corona - GER Wave 3_September 24, 2020_08.40.sav") 
ger_w3_full_df <- filter(dat, Q_Language == "DE" & !is.na(t_end_screen_Page_Submit) & psid != "" & as.Date(EndDate) <= "2020-09-21") # drop observations that came in after last tracking data
ger_w3_full_df$sample_type <- ifelse(ger_w3_full_df$psid %in% tracking_survey_psids, "surveytracking", "surveyonly")
ger_w3_df <- ger_w3_full_df %>% dplyr::select(-starts_with("t_"))
ger_w3_df$wave <- 3
ger_w3_time_df <- ger_w3_full_df %>% dplyr::select(ResponseId, psid, tic, sample_type, starts_with("t_"))
dplyr::select(ger_w3_df, psid, sample_type)  %>% write_xlsx(path = "../data/hertie-corona-ger-w3-completes.xlsx")


# merge waves ------------

# long format
ger_df <- bind_rows(ger_w1_df, ger_w2_df, ger_w3_df)


# prepare message treatment variables ------------

table(ger_df$treatment)
# 1 = pro-social behavior
# 2 = self-interest
# 3 = control

ger_df$treat <- ifelse(ger_df$treatment == "1", "prosocial", ifelse(ger_df$treatment == "2", "selfinterest", "control"))
ger_df$treat_prosoc <- ger_df$treatment == "1"
ger_df$treat_selfint <- ger_df$treatment == "2"
ger_df$treat_any <- (ger_df$treatment == "1" | ger_df$treatment == "2")
ger_df$treat_control <- ger_df$treatment == "3"
ger_df$treat_comp <- ger_df$t_app_intervention_Page_Submit >= 125
ger_df$treat_comp[is.na(ger_df$treat_comp)] <- FALSE


# prepare incentivization treatment variables ----

# incentivization treatment
ger_df$treat_incent <- cut(as.numeric(ger_df$it), breaks=c(0, 1.5, 2.5, 3.5, Inf), labels=c("EUR 1", "EUR 2", "EUR 5", "Control"))
ger_df$treat_incent[is.na(ger_df$t_app_incentive_Page_Submit) & as.numeric(ger_df$it) != 4] <- NA
ger_df$treat_incent[as.numeric(ger_df$it) == 4 & ger_df$app_install %in% c(1, 7)] <- NA
ger_df$treat_incent_eur1 <- ger_df$treat_incent == "EUR 1"
ger_df$treat_incent_eur2 <- ger_df$treat_incent == "EUR 2"
ger_df$treat_incent_eur5 <- ger_df$treat_incent == "EUR 5"
ger_df$treat_incent_control <- ger_df$treat_incent == "Control"
ger_df$treat_incent_any <- (ger_df$treat_incent == "EUR 1" | ger_df$treat_incent == "EUR 2" | ger_df$treat_incent == "EUR 5")

# incentivization compliance: agree to install app
ger_df$treat_incent_agree <- (ger_df$app_incentive1 == 7 | ger_df$app_incentive2 == 7 | ger_df$app_incentive3 == 7)
ger_df$treat_incent_agree[is.na(ger_df$treat_incent_agree) & ger_df$treat_incent_any == TRUE] <- FALSE


# prepare predictor variables ------------

# age
ger_df$age <- 2020 - as.numeric(as.character(as_factor(ger_df$birthyear)))
ger_df$age10 <- ger_df$age/10
ger_df$age_cat <- cut(ger_df$age,breaks = c(0, 29.5, 39.5, 49.5, 59.5, 99), labels = c("18-29", "30-39", "40-49", "50-59", "60-80"), right = FALSE)
ger_df$age_cat5 <- cut(ger_df$age,breaks = c(0, 29.5, 39.5, 49.5, 59.5, 99), labels = c("18-29ys", "30-39ys", "40-49ys", "50-59ys", "60+ys"), right = FALSE)
ger_df$age_cat3 <- cut(ger_df$age,breaks = c(0, 39.5, 59.5, 99), labels = c("18-39ys", "40-59ys", "60+ys"), right = FALSE)
ger_df$age_cat2 <- cut(ger_df$age,breaks = c(0, 49.5, 99), labels = c("18-49ys", "50+ys"), right = FALSE)

# female
ger_df$female <- ger_df$gender == 2
ger_df$gender_cat2 <- cut(ger_df$gender, breaks=c(0, 1.5, 2.5, Inf), labels=c("Male", "Female", "Other"))
ger_df$gender_cat2[ger_df$gender_cat2 == "Other"] <- NA

# education, categorical
ger_df$educ_cat <- cut(ger_df$education, breaks=c(0, 2.5, 3.5, Inf), labels=c("lo", "mid", "hi"))
ger_df$educ_cat3 <- cut(ger_df$education, breaks=c(0, 2.5, 3.5, Inf), labels=c("Low", "Medium", "High"))
ger_df$edu_lo <- ger_df$educ_cat == "lo"
ger_df$edu_mid <- ger_df$educ_cat == "mid"
ger_df$edu_hi <- ger_df$educ_cat == "hi"

# number of children
ger_df$children_num <- ger_df$children - 1
ger_df$children_cat2 <- cut(ger_df$children_num, breaks=c(-Inf, 0.5, Inf), labels=c("No", "Yes"))
ger_df$children_yes <- ifelse(ger_df$children_num > 0, TRUE, FALSE)

# household income
ger_df$hhincome_rec <- ger_df$hhincome
ger_df$hhincome_rec[ger_df$hhincome_rec == 7] <- 5
ger_df$hhincome_rec[ger_df$hhincome_rec == 8] <- 6
ger_df$hhinc_cat <- recode_factor(as.factor(ger_df$hhincome_rec), `1` = "Under EUR 500", `2` = "EUR 500 to EUR 1,499", `3` = "EUR 1,500 to EUR 2,999", `4` = "EUR 3,000 to EUR 4,999", `5` = "EUR 5,000 to EUR 9,999", `6` = "EUR 10,000 or over")
ger_df$hhinc_cat3 <- cut(ger_df$hhincome_rec, breaks=c(0, 2.5, 3.5, Inf), labels=c("<1.5k", "1.5-3k", ">3k"))
ger_df$inc_lo <- ger_df$hhinc_cat %in% c("Under EUR 500", "EUR 500 to EUR 1,499")
ger_df$inc_mid <- ger_df$hhinc_cat %in% c("EUR 1,500 to EUR 2,999")
ger_df$inc_hi <- ger_df$hhinc_cat %in% c("EUR 3,000 to EUR 4,999", "EUR 5,000 to EUR 9,999", "EUR 10,000 or over")

# living in region with high number of cases; urban region
covidcases_df <- read_csv("../data/cases_landkreis.csv")
covidcases_df <- distinct(covidcases_df, PLZ, .keep_all = TRUE)
covidcases_df$pop100k <- covidcases_df$cases  / covidcases_df$cases_per_100k
covidcases_df$zipcode <- as.character(covidcases_df$PLZ)
covidcases_df$zipcode[str_length(covidcases_df$zipcode) == 4] <- paste0("0", covidcases_df$zipcode[str_length(covidcases_df$zipcode) == 4])
covidcases_df <- dplyr::select(covidcases_df, zipcode, BL, Kreis, Typ, cases_per_100k, pop100k)
ger_df <- merge(ger_df, covidcases_df, by = "zipcode", all.x = TRUE)
dat_grouped <- group_by(covidcases_df, zipcode) %>% summarize(mean_cases = mean(cases_per_100k, na.rm = TRUE),
                                                              mean_pop = mean(pop100k, na.rm = TRUE))
quantile(dat_grouped$mean_cases, seq(0, 1, .1), na.rm = TRUE)
cases90perc <- quantile(dat_grouped$mean_cases, seq(0, 1, .1), na.rm = TRUE)[10]

quantile(log(dat_grouped$mean_pop), seq(0, 1, .1), na.rm = TRUE)
logpop90perc <- quantile(log(dat_grouped$mean_pop), seq(0, 1, .1), na.rm = TRUE)[10]

ger_df$cases90perc <- ger_df$cases_per_100k >= cases90perc
ger_df$cases90perc_cat2 <- ifelse(ger_df$cases90perc == TRUE, "Yes", "No")
ger_df$urban <- ger_df$Typ == "Stadt"
ger_df$urban_cat2 <- cut(as.numeric(ger_df$urban), breaks=c(-Inf, 0.5, Inf), labels=c("No", "Yes"))
  
# job status
ger_df$at_work <- ger_df$job_status %in% c(2, 3, 5, 6) 
ger_df$working_cat2 <- cut(as.numeric(ger_df$at_work), breaks=c(-Inf, 0.5, Inf), labels=c("No", "Yes"))
ger_df$working_cat2[is.na(ger_df$job_status)] <- NA

# trust in government (0 = no trust at all, 4 = complete trust)
ger_df$trust_gov <- recode(as.numeric(ger_df$trust_institutions_1), `1` = 1, `3` = 2, `4` = 4, `5` = 5, `9` = 3) - 1
ger_df$trust_gov_cat3 <- cut(ger_df$trust_gov, breaks=c(-Inf, 1.5, 2.5, Inf), labels=c("Low", "Medium", "High"))
ger_df$trust_gov_cat2 <- ger_df$trust_gov_cat3
ger_df$trust_gov_cat2[ger_df$trust_gov_cat2 == "Medium"] <- NA
ger_df$trusts_gov <- ger_df$trust_gov_cat3 == "High"
ger_df$trusts_gov[ger_df$trust_gov_cat3 == "Medium"] <- NA

# trust in scientific experts  (0 = no trust at all, 4 = complete trust)
ger_df$trust_sci <- recode(as.numeric(ger_df$trust_institutions_3), `1` = 1, `3` = 2, `4` = 4, `5` = 5, `9` = 3) - 1
ger_df$trust_sci_cat3 <- cut(ger_df$trust_sci, breaks=c(-Inf, 1.5, 2.5, Inf), labels=c("Low", "Medium", "High"))
ger_df$trust_sci_cat2 <- ger_df$trust_sci_cat3
ger_df$trust_sci_cat2[ger_df$trust_sci_cat2 == "Medium"] <- NA
ger_df$trusts_sci <- ger_df$trust_sci_cat3 == "High"
ger_df$trusts_sci[ger_df$trust_sci_cat3 == "Medium"] <- NA

# trust in healthcare system  (0 = no trust at all, 4 = complete trust)
ger_df$trust_hcs <- recode(as.numeric(ger_df$trust_institutions_5), `1` = 1, `3` = 2, `4` = 4, `5` = 5, `9` = 3) - 1
ger_df$trust_hcs_cat3 <- cut(ger_df$trust_hcs, breaks=c(-Inf, 1.5, 2.5, Inf), labels=c("Low", "Medium", "High"))
ger_df$trust_hcs_cat2 <- ger_df$trust_hcs_cat3
ger_df$trust_hcs_cat2[ger_df$trust_hcs_cat2 == "Medium"] <- NA
ger_df$trusts_hcs <- ger_df$trust_hcs_cat3 == "High"
ger_df$trusts_hcs[ger_df$trust_hcs_cat3 == "Medium"] <- NA

# concernedness about COVID-19 for themselves
ger_df$covidconcerned_self <- ger_df$corona_concerned_1 - 1
ger_df$covidconcerned_self_cat2 <- cut(ger_df$covidconcerned_self, breaks=c(-Inf, 1.5, Inf), labels=c("Low", "High"))
  
# concernedness about COVID-19 for family/friends
ger_df$covidconcerned_famf <- ger_df$corona_concerned_2 - 1
ger_df$covidconcerned_famf_cat2 <- cut(ger_df$covidconcerned_famf , breaks=c(-Inf, 1.5, Inf), labels=c("Low", "High"))

# incidence of COVID-19 infections in personal environment
ger_df$covidcase <- ger_df$corona_infected == 2 | ger_df$corona_infected == 3
ger_df$covidcase_cat2 <- ifelse(ger_df$covidcase == TRUE, "Yes", "No")

# general state of health
ger_df$health_cond <- ger_df$health_condition - 1

# pre-existing health conditions
ger_df$precond <- ger_df$precondition == 1
ger_df$precond_cat2 <- ifelse(ger_df$precond == TRUE, "Yes", "No")

# social responsibility score
ger_df$soc_responsibility_1_rec <- recode(as.numeric(ger_df$soc_responsibility_1), `1` = 1, `2` = 2, `7` = 3, `3` = 4) 
ger_df$soc_responsibility_2_rec <- recode(as.numeric(ger_df$soc_responsibility_2), `1` = 1, `2` = 2, `7` = 3, `3` = 4) 
ger_df$soc_responsibility_3_rec <- recode(as.numeric(ger_df$soc_responsibility_3), `1` = 1, `2` = 2, `7` = 3, `3` = 4) 
socresp_scale_fit <- psych::principal(filter(ger_df, wave == 1) %>% dplyr::select(soc_responsibility_1_rec, soc_responsibility_2_rec, soc_responsibility_3_rec), nfactors = 1, rotate = "varimax", missing = TRUE, impute = "mean")
ger_df$socresp_score <- predict(socresp_scale_fit, dplyr::select(ger_df, soc_responsibility_1_rec, soc_responsibility_2_rec, soc_responsibility_3_rec))[,1]
socresp_score_quantiles <- quantile(ger_df$socresp_score, probs = c(0, .33, .66, 1), na.rm = TRUE)
ger_df$socresp_score_cat3 <- cut(ger_df$socresp_score,breaks = c(-Inf, socresp_score_quantiles[2], socresp_score_quantiles[3], Inf), labels = c("Low", "Medium", "High"), right = FALSE)

# self interest score
ger_df$soc_responsibility_4_rec <- recode(as.numeric(ger_df$soc_responsibility_4), `1` = 1, `2` = 2, `7` = 3, `3` = 4) 
ger_df$soc_responsibility_5_rec <- recode(as.numeric(ger_df$soc_responsibility_5), `1` = 1, `2` = 2, `7` = 3, `3` = 4) 
ger_df$soc_responsibility_6_rec <- recode(as.numeric(ger_df$soc_responsibility_6), `1` = 1, `2` = 2, `7` = 3, `3` = 4) 
selfint_scale_fit <- psych::principal(filter(ger_df, wave == 1) %>% dplyr::select(soc_responsibility_4_rec, soc_responsibility_5_rec, soc_responsibility_6_rec), nfactors = 1, rotate = "varimax", missing = TRUE, impute = "mean")
ger_df$selfint_score <- predict(selfint_scale_fit, dplyr::select(ger_df, soc_responsibility_4_rec, soc_responsibility_5_rec, soc_responsibility_6_rec))[,1]
selfint_score_quantiles <- quantile(ger_df$selfint_score, probs = c(0, .33, .66, 1), na.rm = TRUE)
ger_df$selfint_score_cat3 <- cut(ger_df$selfint_score,breaks = c(-Inf, selfint_score_quantiles[2], selfint_score_quantiles[3], Inf), labels = c("Low", "Medium", "High"), right = FALSE)

# data privacy concerns
ger_df$data_control_1_rec <- recode(as.numeric(ger_df$data_control_1), `6` = 1, `7` = 2, `8` = 3, `9` = 4) 
ger_df$data_control_2_rec <- recode(as.numeric(ger_df$data_control_2), `6` = 1, `7` = 2, `8` = 3, `9` = 4) 
ger_df$data_control_3_rec <- recode(as.numeric(ger_df$data_control_3), `6` = 1, `7` = 2, `8` = 3, `9` = 4) 
datacontrol_scale_fit <- psych::principal(filter(ger_df, wave == 1) %>% dplyr::select(data_control_1_rec, data_control_2_rec, data_control_3_rec), nfactors = 2, rotate = "varimax", missing = TRUE, impute = "mean")
ger_df$datacontrol_score <- predict(datacontrol_scale_fit, dplyr::select(ger_df, data_control_1_rec, data_control_2_rec, data_control_3_rec))[,1]
datacontrol_score_quantiles <- quantile(ger_df$datacontrol_score, probs = c(0, .33, .66, 1), na.rm = TRUE)
ger_df$datacontrol_score_cat3 <- cut(ger_df$datacontrol_score,breaks = c(-Inf, datacontrol_score_quantiles[2], datacontrol_score_quantiles[3], Inf), labels = c("Low", "Medium", "High"), right = FALSE)

# digital literacy
ger_df$digital_literacy_1_rec <- recode(as.numeric(ger_df$digital_literacy_1), `9` = 1, `15` = 2, `12` = 3, `15` = 4) 
ger_df$digital_literacy_2_rec <- recode(as.numeric(ger_df$digital_literacy_2), `9` = 1, `15` = 2, `12` = 3, `15` = 4) 
ger_df$digital_literacy_3_rec <- recode(as.numeric(ger_df$digital_literacy_3), `9` = 1, `15` = 2, `12` = 3, `15` = 4) 
ger_df$digital_literacy_4_rec <- recode(as.numeric(ger_df$digital_literacy_4), `9` = 1, `15` = 2, `12` = 3, `15` = 4) 

digliteracy_scale_fit <- psych::principal(filter(ger_df, wave == 1) %>% dplyr::select(digital_literacy_1_rec, digital_literacy_2_rec, digital_literacy_3_rec, digital_literacy_4_rec), nfactors = 1, rotate = "varimax", missing = TRUE, impute = "mean")
ger_df$digliteracy_score <- predict(digliteracy_scale_fit, dplyr::select(ger_df, digital_literacy_1_rec, digital_literacy_2_rec, digital_literacy_3_rec, digital_literacy_4_rec))[,1]
digliteracy_score_quantiles <- quantile(ger_df$digliteracy_score, probs = c(0, .33, .66, 1), na.rm = TRUE)
ger_df$digliteracy_score_cat3 <- cut(ger_df$digliteracy_score,breaks = c(-Inf, digliteracy_score_quantiles[2], digliteracy_score_quantiles[3], Inf), labels = c("Low", "Medium", "High"), right = FALSE)
digliteracy_score_quantiles <- quantile(ger_df$digliteracy_score, probs = c(0, .5, 1), na.rm = TRUE)
ger_df$digliteracy_score_cat2 <- cut(ger_df$digliteracy_score,breaks = c(-Inf, digliteracy_score_quantiles[2], Inf), labels = c("Low", "High"), right = FALSE)

# risk behavior
ger_df$behav_pubtransport <- factor(as_factor(ger_df$risky_behavior_1), levels = c("Never", "Once this week", "Several times this week", "Every day", "Don't know"))
ger_df$behav_restaurant <- factor(as_factor(ger_df$risky_behavior_2), levels = c("Never", "Once this week", "Several times this week", "Every day", "Don't know"))
ger_df$behav_visitfriends <- factor(as_factor(ger_df$risky_behavior_3), levels = c("Never", "Once this week", "Several times this week", "Every day", "Don't know"))
ger_df$behav_pubtransport_cat2 <- cut(as.numeric(ger_df$behav_pubtransport),breaks = c(-Inf, 2.5, 4.5, Inf), labels = c("Low", "High", "Don't know"), right = FALSE)
ger_df$behav_pubtransport_cat2[ger_df$behav_pubtransport_cat2 == "Don't know"] <- NA
ger_df$behav_restaurant_cat2 <- cut(as.numeric(ger_df$behav_restaurant),breaks = c(-Inf, 2.5, 4.5, Inf), labels = c("Low", "High", "Don't know"), right = FALSE)
ger_df$behav_restaurant_cat2[ger_df$behav_restaurant_cat2 == "Don't know"] <- NA
ger_df$behav_visitfriends_cat2 <- cut(as.numeric(ger_df$behav_visitfriends),breaks = c(-Inf, 2.5, 4.5, Inf), labels = c("Low", "High", "Don't know"), right = FALSE)
ger_df$behav_visitfriends_cat2[ger_df$behav_visitfriends_cat2 == "Don't know"] <- NA
ger_df$behav_pubtransport_num2 <- as.numeric(ger_df$behav_pubtransport_cat2) - 1
ger_df$behav_restaurant_num2 <- as.numeric(ger_df$behav_restaurant_cat2) - 1
ger_df$behav_visitfriends_num2 <- as.numeric(ger_df$behav_visitfriends_cat2) - 1

ger_df$use_pubtransport <- as.numeric(ger_df$behav_pubtransport)
ger_df$use_pubtransport[ger_df$use_pubtransport == 5] <- NA
ger_df$visit_restaurant <- as.numeric(ger_df$behav_restaurant)
ger_df$visit_restaurant[ger_df$visit_restaurant == 5] <- NA
ger_df$visit_friends <- as.numeric(ger_df$behav_visitfriends)
ger_df$visit_friends[ger_df$visit_friends == 5] <- NA
riskybehav_scale_fit <- psych::principal(ger_df %>% dplyr::select(use_pubtransport, visit_restaurant, visit_friends), nfactors = 2, rotate = "varimax", missing = TRUE, impute = "mean")
ger_df$riskybehav_score <- predict(riskybehav_scale_fit, dplyr::select(ger_df, use_pubtransport, visit_restaurant, visit_friends))[,1]
riskybehav_score_quantiles <- quantile(ger_df$riskybehav_score, probs = c(0, .33, .66, 1), na.rm = TRUE)
ger_df$riskybehav_score_cat3 <- cut(ger_df$riskybehav_score,breaks = c(-Inf, riskybehav_score_quantiles[2], riskybehav_score_quantiles[3], Inf), labels = c("Low", "Medium", "High"), right = FALSE)
  
# access to smartphone (dropped later because of perfect collinearity)
ger_df$smartphone_access <- ger_df$smartphone %in% c(1, 2, 3)

# attitude towards Anti-COVID measures
ger_df$covidmeasures_support <- ger_df$corona_measures - 1
ger_df$covidmeasures_support[ger_df$covidmeasures_support == 5] <- NA

# compliance with Anti-COVID rules
ger_df$aha_compliance <- ger_df$corona_precautions == 1
ger_df$aha_compliance_cat2 <- ifelse(ger_df$aha_compliance == TRUE, "Yes", "No")


# attention check
ger_df$attcheck_pass <- ifelse(ger_df$attcheck_treatment_1 == 1 & 
                                    ger_df$attcheck_treatment_11 == 1 &
                                    is.na(ger_df$attcheck_treatment_2) & 
                                    is.na(ger_df$attcheck_treatment_3) & 
                                    is.na(ger_df$attcheck_treatment_4) & 
                                    is.na(ger_df$attcheck_treatment_5) & 
                                    is.na(ger_df$attcheck_treatment_6) & 
                                    is.na(ger_df$attcheck_treatment_7) & 
                                    is.na(ger_df$attcheck_treatment_8) & 
                                    is.na(ger_df$attcheck_treatment_9) & 
                                    is.na(ger_df$attcheck_treatment_10) & 
                                    is.na(ger_df$attcheck_treatment_12), TRUE, FALSE)



# prepare outcome variables ------------

# app currently installed (binary)
ger_df$app_install_current <- ger_df$app_install == 1

# app initially used (binary)
ger_df$app_install_initial <- ger_df$app_install == 1 | ger_df$app_install == 3

# Bluetooth activated (numeric 1-5, never-always)
tabyl(as_factor(ger_df$app_bluetooth))
ger_df$app_bluetooth_act <- 6 - ger_df$app_bluetooth

# probability to submit positive test result in app (numeric 1-7, certainly no-certainly yes)
ger_df$app_submit_positive_result <- ger_df$app_positive_test
ger_df$app_submit_positive_result[ger_df$app_submit_positive_result == 8] <- NA

# probability of testing for COVID-19 after getting app risk warning (numeric 1-7, certainly no-certainly yes)
ger_df$app_test_warning <- ger_df$app_risk_reaction1
ger_df$app_test_warning[ger_df$app_test_warning == 8] <- NA

# attitudes about app (PCA fit on W1; predict W2 index using weights from W1); the higher, the more positive attitudes
att_scale_fit <- psych::principal(filter(ger_df, wave == 1) %>% dplyr::select(app_attitudes_1, app_attitudes_2, app_attitudes_3, app_attitudes_4), nfactors = 1, rotate = "varimax", missing=TRUE, impute = "mean")

ger_df$att_scale_score <- - predict(att_scale_fit, ger_df %>% dplyr::select(app_attitudes_1, app_attitudes_2, app_attitudes_3, app_attitudes_4)) # make negative to reflect positive attitudes in higher values
att_scale_score_quantiles <- quantile(ger_df$att_scale_score, probs = c(0, .33, .66, 1), na.rm = TRUE)
ger_df$att_scale_score_cat3 <- cut(ger_df$att_scale_score,breaks = c(-Inf, att_scale_score_quantiles[2], att_scale_score_quantiles[3], Inf), labels = c("Low", "Medium", "High"), right = FALSE)

# knowledge about app
ger_df$app_knowledge_1_cor <- ger_df$app_knowledge_1 == 1 
ger_df$app_knowledge_2_cor <- ger_df$app_knowledge_2 == 4 # manipulation check prosocial behavior treatment
ger_df$app_knowledge_3_cor <- ger_df$app_knowledge_3 == 4 
ger_df$app_knowledge_4_cor <- ger_df$app_knowledge_4 == 1 
ger_df$app_knowledge_5_cor <- ger_df$app_knowledge_5 == 4 # manipulation check self-interest treatment
ger_df$app_know_index <- dplyr::select(ger_df, app_knowledge_1_cor, app_knowledge_2_cor, app_knowledge_3_cor, app_knowledge_4_cor, app_knowledge_5_cor) %>% rowSums()
ger_df$app_know_index_cat <- cut(ger_df$app_know_index, breaks = c(-Inf, 1.5, 3.5, Inf), labels = c("Low", "Medium", "High"), right = FALSE)

# app icon knowledge
ger_df$app_know_icon_cor <- ger_df$app_logo == 3
ger_df$app_know_screen_cor <- ger_df$app_screenshot == 1
ger_df$app_know2_index <- dplyr::select(ger_df, app_know_icon_cor, app_know_screen_cor) %>% rowSums()

# sharing behavior: FB, Twitter, WhatsApp, Email
clicks_df <-  dplyr::select(ger_df, clicked_fb, clicked_tw, clicked_whatsapp, clicked_mailto) %>% mutate(across(where(is.character),as.numeric))
ger_df$app_sharing_clicks <- rowSums(clicks_df)
  
# information behavior: information resources, App Store / Google Playstore
clicks2_df <-  dplyr::select(ger_df, clicked_appstore, clicked_playstore, clicked_vbzentrale, clicked_ccc, clicked_merkel, clicked_erklaervideo) %>% mutate(across(where(is.character),as.numeric))
ger_df$app_info_clicks <- rowSums(clicks2_df)

# manipulation check
ger_df$manipcheck_pass <- ifelse((ger_df$treat_prosoc == TRUE & ger_df$app_knowledge_2_cor == TRUE) |
                                   (ger_df$treat_selfint == TRUE & ger_df$app_knowledge_5_cor == TRUE) | 
                                   ger_df$treat_any == FALSE, TRUE, FALSE)

# collect outcome vars
outcome_vars <- c("app_install_current", "app_install_initial", "app_bluetooth_act", "app_likely_install", "app_submit_positive_result", "app_test_warning", "att_scale_score", "app_know_index", "app_know2_index", "app_knowledge_2_cor", "app_knowledge_5_cor", "app_sharing_clicks", "app_info_clicks", "app_test_warning", "app_submit_positive_result")


# standardize outcome variables ------------

for (i in outcome_vars){
  ger_df[,paste0(i, "_std")] <- ger_df[,i]/sd(ger_df[,i][which(ger_df$treat == "control")], na.rm = TRUE)
  ger_df[,paste0(i, "_incent_std")] <- ger_df[,i]/sd(ger_df[,i][which(ger_df$treat_incent == "Control")], na.rm = TRUE)
}


# prepare tracking sociodemographics data ------------

# age
tracking_sociodem_df$age_cat <- cut(tracking_sociodem_df$age,breaks = c(0, 29.5, 39.5, 49.5, 59.5, 99), labels = c("18-29", "30-39", "40-49", "50-59", "60-80"), right = FALSE)
tracking_sociodem_df$age_cat3 <- cut(tracking_sociodem_df$age,breaks = c(0, 39.5, 59.5, 99), labels = c("18-39ys", "40-59ys", "60+ys"), right = FALSE)


# female
tracking_sociodem_df$female <- tracking_sociodem_df$gender == 2

# education, categorical
tracking_sociodem_df$education[tracking_sociodem_df$education == 4] <- 1
tracking_sociodem_df$educ_cat <- cut(tracking_sociodem_df$education, breaks=c(0, 1.5, 2.5, Inf), labels=c("lo", "mid", "hi"))

# number of children
tracking_sociodem_df$`household number of children`[tracking_sociodem_df$`household number of children` == 5] <- 4
tracking_sociodem_df$children_num <- tracking_sociodem_df$`household number of children`
tracking_sociodem_df$children_yes <- ifelse(tracking_sociodem_df$children_num > 0, TRUE, FALSE)


# household income
tracking_sociodem_df$`household income`[tracking_sociodem_df$`household income` %in% c(98, 99)] <- NA
tracking_sociodem_df$hhincome_rec <- tracking_sociodem_df$`household income`
tracking_sociodem_df <- tracking_sociodem_df %>% mutate(hhincome_rec = case_when(
  hhincome_rec == 1 ~ 1,
  hhincome_rec %in% c(2, 3) ~ 2,
  hhincome_rec %in% c(4, 5, 6) ~ 3,
  hhincome_rec %in% c(7, 8, 9, 10) ~ 4,
  hhincome_rec == 11 ~ 5
))
tracking_sociodem_df$hhinc_cat <- recode_factor(as.factor(tracking_sociodem_df$hhincome_rec), `1` = "Under EUR 500", `2` = "EUR 500 to EUR 1,499", `3` = "EUR 1,500 to EUR 2,999", `4` = "EUR 3,000 to EUR 4,999", `5` = "EUR 5,000 to EUR 9,999", `6` = "EUR 10,000 or over")
tracking_sociodem_df$hhinc_cat3 <- cut(tracking_sociodem_df$hhincome_rec, breaks=c(0, 2.5, 3.5, Inf), labels=c("<1.5k", "1.5-3k", ">3k"))

# job status
tracking_sociodem_df$at_work <- tracking_sociodem_df$employment %in% c(1, 2, 3, 4, 5, 6)

# access to smartphone (dropped later because of perfect collinearity)
tracking_sociodem_df$smartphone_access <- TRUE

# keep tracking-only participants only
tracking_only_df <- filter(tracking_sociodem_df, psid %in% tracking_only_psids) %>% dplyr::select(psid, age_cat, female, educ_cat, children_num, hhinc_cat, at_work)

# create additional variables for merging
tracking_only_df$psid <- as.character(tracking_only_df$psid)
tracking_only_df$sample_type = "trackingonly"

# treatment variables
tracking_only_df$treat <- "control"
tracking_only_df$treat_prosoc <- FALSE
tracking_only_df$treat_selfint <- FALSE
tracking_only_df$treat_any <- FALSE
tracking_only_df$treat_control <- TRUE
tracking_only_df$treat_comp <- FALSE

tracking_only_df$treat_incent <- "Control"
tracking_only_df$treat_incent_any <- FALSE
tracking_only_df$treat_incent_agree <- NA
tracking_only_df$treat_incent_eur1  <- FALSE
tracking_only_df$treat_incent_eur2  <- FALSE
tracking_only_df$treat_incent_eur5  <- FALSE
tracking_only_df$treat_incent_control  <- TRUE


# clone data frame to mirror wave logic below
tracking_only_df_long <- bind_rows(
  mutate(tracking_only_df, wave = 1, StartDate = min(ger_w1_df$StartDate)), 
  mutate(tracking_only_df, wave = 2, StartDate = min(ger_w2_df$StartDate)),
  mutate(tracking_only_df, wave = 3, StartDate = min(ger_w3_df$StartDate)))


# prepare tracking data ------------

# compute days
date_app_launch <- "20200615"
tracking_df$app_usage_days_tot <- as.numeric(ymd(date_data) - ymd(as.Date(tracking_df$used_at)))
tracking_df$app_install_days_since_launch <- ymd(as.Date(tracking_df$used_at)) - as.numeric(ymd(date_app_launch))
tracking_df$app_used_7days_before_w1 <- ymd(as.Date(tracking_df$used_at)) >= (min(ymd(as.Date(ger_w1_df$StartDate)), na.rm = TRUE) - 7) & ymd(as.Date(tracking_df$used_at)) < min(ymd(as.Date(ger_w1_df$StartDate)), na.rm = TRUE)
tracking_df$app_used_7days_before_w3 <- ymd(as.Date(tracking_df$used_at)) >= (min(ymd(as.Date(ger_w1_df$StartDate)), na.rm = TRUE) - 7) & ymd(as.Date(tracking_df$used_at)) < min(ymd(as.Date(ger_w3_df$StartDate)), na.rm = TRUE)


# compute usage statistics
tracking_df_vars <- group_by(tracking_df, psid) %>% 
  summarize(track_installed = TRUE,
            track_n_devices = length(unique(device_id)),
            track_date_first_install = min(used_at),
            track_install_days_since_launch = as.numeric(as.Date(track_date_first_install) - ymd(date_app_launch)),
            track_usage_days_tot = mean(app_usage_days_tot) + 1, # avoid div by 0 for track_usage_n_daily
            track_usage_n_tot = n(),
            track_usage_n_daily = track_usage_n_tot / track_usage_days_tot,
            track_usage_daily_avg_7days_before_w1 = sum(app_used_7days_before_w1) / 7,
            track_usage_daily_avg_7days_before_w3 = sum(app_used_7days_before_w3) / 7,
            track_usage_time_avg = mean(duration, na.rm = TRUE),
            track_usage_time_tot = sum(duration, na.rm = TRUE),
            device_id = first(device_id))

# merge with device information
tracking_df_all <- merge(dplyr::select(tracking_df_vars, -psid), devices_df, by = "device_id", all = TRUE)
tracking_df_all$track_installed[is.na(tracking_df_all$track_installed)] <- FALSE
tracking_df_all$track_date_first_install[is.na(tracking_df_all$track_date_first_install)] <- as.Date("9999-01-01")

# delete duplicates due to multiple devices per respondent (but keep those on which tracking data is installed)
tracking_df_all <- tracking_df_all %>% arrange(desc(track_installed)) %>% distinct(psid, .keep_all = TRUE)

# number of observations from tracking survey df for which we don't have device info
length(setdiff(tracking_survey_psids, tracking_df_all$psid))


# combine survey/survey-tracking with tracking participants data ----------------------------

ger_df_track <- bind_rows(ger_df, tracking_only_df_long)


# merge survey and tracking data ----------------------------

ger_df_track_all <- merge(ger_df_track, tracking_df_all, by = "psid", all.x = TRUE)


# classify Apple device users to "survey-only" group ----------
## Background: due to a mistake by the tracking data provider, app usage on Apple devices was not tracked

apple_remove <- TRUE

ger_df_wide$apple <- ger_df_wide$device_manufacturer.1 == "Apple"
ger_df_wide$datacontrol_score_cat3.1
with(ger_df_wide, prop.table(table(datacontrol_score_cat3.1, apple)))

if(apple_remove == TRUE) {

# remove tracking-only Apple users
ger_df_track_all <- filter(ger_df_track_all, !(sample_type == "trackingonly" & device_manufacturer == "Apple"))

# make sample type of survey-tracking users with Apple devices "surveyonly"
ger_df_track_all$sample_type[ger_df_track_all$device_manufacturer == "Apple" & ger_df_track_all$sample_type == "surveytracking"] <- "surveyonly"

# apple device indicator
ger_df_track_all$apple <- ger_df_track_all$device_manufacturer == "Apple"

# set tracking variables to missing
track_vars <- str_subset(names(ger_df_track_all), "^track_")
for(i in track_vars){
  ger_df_track_all[,i][ger_df_track_all$apple == TRUE] <- NA
}
}else{
  ger_df_track_all$apple <- ger_df_track_all$device_manufacturer == "Apple"
  
}


# make df wide ----------------------------

ger_w1_df_w <- filter(ger_df_track_all, wave == 1)
names(ger_w1_df_w) <- paste0(names(ger_w1_df_w), ".1")
ger_w2_df_w <- filter(ger_df_track_all, wave == 2)
names(ger_w2_df_w) <- paste0(names(ger_w2_df_w), ".2")
ger_w3_df_w <- filter(ger_df_track_all, wave == 3)
names(ger_w3_df_w) <- paste0(names(ger_w3_df_w), ".3")
ger_df_wide <- merge(ger_w1_df_w, ger_w2_df_w, by.x = "psid.1", by.y = "psid.2", all = TRUE) %>% 
  merge(ger_w3_df_w, by.x = "psid.1", by.y = "psid.3", all = TRUE)


# get rid of some duplicates
#filter(ger_df_wide, psid.1 %in% ger_df_wide$psid.1[ger_df_wide$dup == TRUE]) %>% dplyr::select(sample_type.1, dup, psid.1, track_installed.1, app_know_index.1, app_install_initial.1) %>% View()
ger_df_wide <- ger_df_wide %>% distinct(psid.1, .keep_all = TRUE) %>% filter(!is.na(sample_type.1))


# re-specify tracking app usage indicators ------------

# installed at...
ger_df_wide$app_track_installed_at_w1 <- (ger_df_wide$track_date_first_install.1 < ger_df_wide$StartDate.1)
ger_df_wide$app_track_installed_at_w2 <- (ger_df_wide$track_date_first_install.1 < ger_df_wide$StartDate.2)
ger_df_wide$app_track_installed_at_w3 <- (ger_df_wide$track_date_first_install.1 < ger_df_wide$StartDate.3)

# installed earlier than...
ger_df_wide$app_track_installed_before_w1 <- (ger_df_wide$track_date_first_install.1 < ger_df_wide$StartDate.1)
ger_df_wide$app_track_installed_before_w2 <- (ger_df_wide$track_date_first_install.1 < ger_df_wide$StartDate.2)
ger_df_wide$app_track_installed_before_w3 <- (ger_df_wide$track_date_first_install.1 < ger_df_wide$StartDate.3)

# installed between...
ger_df_wide$app_track_installed_between_w1w2 <- (ger_df_wide$track_date_first_install.1 >= ger_df_wide$StartDate.1) & (ger_df_wide$track_date_first_install.1 <= ger_df_wide$StartDate.2)
ger_df_wide$app_track_installed_between_w2w3 <- (ger_df_wide$track_date_first_install.1 >= ger_df_wide$StartDate.2) & (ger_df_wide$track_date_first_install.1 <= ger_df_wide$StartDate.3) 


# re-specify survey app usage indicators ------------

# installed at...
ger_df_wide$app_rep_installed_at_w1 <- ger_df_wide$app_install.1 == 1 
ger_df_wide$app_rep_installed_at_w2 <- ger_df_wide$app_install.2 == 1 
ger_df_wide$app_rep_installed_at_w3 <- ger_df_wide$app_install.3 == 1 

# installed earlier than...
ger_df_wide$app_rep_installed_before_w1 <- ger_df_wide$app_install.1 == 1 | ger_df_wide$app_install.1 == 3
ger_df_wide$app_rep_installed_before_w2 <- ger_df_wide$app_install.2 == 1 | ger_df_wide$app_install.2 == 3
ger_df_wide$app_rep_installed_before_w3 <- ger_df_wide$app_install.3 == 1 | ger_df_wide$app_install.3 == 3

# installed between...
ger_df_wide$app_rep_installed_between_w1w2 <- ger_df_wide$app_install.1 %in% c(2, 3, 7) & ger_df_wide$app_install.2 == 1
ger_df_wide$app_rep_installed_between_w2w3 <- ger_df_wide$app_install.2 %in% c(2, 3, 7) & ger_df_wide$app_install.3 == 1

# set those to FALSE who don't have access to smartphone 
ger_df_wide$app_rep_installed_at_w1[ger_df_wide$smartphone.1 == 4] <- FALSE
ger_df_wide$app_rep_installed_at_w2[ger_df_wide$smartphone.1 == 4] <- FALSE
ger_df_wide$app_rep_installed_at_w3[ger_df_wide$smartphone.1 == 4] <- FALSE
ger_df_wide$app_rep_installed_before_w1[ger_df_wide$smartphone.1 == 4] <- FALSE
ger_df_wide$app_rep_installed_before_w2[ger_df_wide$smartphone.1 == 4] <- FALSE
ger_df_wide$app_rep_installed_before_w3[ger_df_wide$smartphone.1 == 4] <- FALSE
ger_df_wide$app_rep_installed_between_w1w2[ger_df_wide$smartphone.1 == 4] <- FALSE
ger_df_wide$app_rep_installed_between_w2w3[ger_df_wide$smartphone.1 == 4] <- FALSE


# build combined app usage indicators ------------

ger_df_wide$app_hyb_installed_at_w1 <- ger_df_wide$app_rep_installed_at_w1 
ger_df_wide$app_hyb_installed_at_w2 <- ger_df_wide$app_rep_installed_at_w2
ger_df_wide$app_hyb_installed_at_w3 <- ger_df_wide$app_rep_installed_at_w3 
ger_df_wide$app_hyb_installed_before_w1 <- ger_df_wide$app_rep_installed_before_w1 
ger_df_wide$app_hyb_installed_before_w2 <- ger_df_wide$app_rep_installed_before_w2
ger_df_wide$app_hyb_installed_before_w3 <- ger_df_wide$app_rep_installed_before_w3 
ger_df_wide$app_hyb_installed_between_w1w2 <- ger_df_wide$app_rep_installed_between_w1w2 
ger_df_wide$app_hyb_installed_between_w2w3 <- ger_df_wide$app_rep_installed_between_w2w3 

ger_df_wide$app_hyb_installed_at_w1[ger_df_wide$sample_type.1 != "surveyonly" & !is.na(ger_df_wide$app_track_installed_at_w1)] <- ger_df_wide$app_track_installed_at_w1[ger_df_wide$sample_type.1 != "surveyonly" & !is.na(ger_df_wide$app_track_installed_at_w1)]
ger_df_wide$app_hyb_installed_at_w2[ger_df_wide$sample_type.1 != "surveyonly" & !is.na(ger_df_wide$app_track_installed_at_w2)] <- ger_df_wide$app_track_installed_at_w2[ger_df_wide$sample_type.1 != "surveyonly" & !is.na(ger_df_wide$app_track_installed_at_w2)]
ger_df_wide$app_hyb_installed_at_w3[ger_df_wide$sample_type.1 != "surveyonly" & !is.na(ger_df_wide$app_track_installed_at_w3)] <- ger_df_wide$app_track_installed_at_w3[ger_df_wide$sample_type.1 != "surveyonly" & !is.na(ger_df_wide$app_track_installed_at_w3)]

ger_df_wide$app_hyb_installed_before_w1[ger_df_wide$sample_type.1 != "surveyonly" & !is.na(ger_df_wide$app_track_installed_before_w1)] <- ger_df_wide$app_track_installed_before_w1[ger_df_wide$sample_type.1 != "surveyonly" & !is.na(ger_df_wide$app_track_installed_before_w1)]
ger_df_wide$app_hyb_installed_before_w2[ger_df_wide$sample_type.1 != "surveyonly" & !is.na(ger_df_wide$app_track_installed_before_w2)] <- ger_df_wide$app_track_installed_before_w2[ger_df_wide$sample_type.1 != "surveyonly" & !is.na(ger_df_wide$app_track_installed_before_w2)]
ger_df_wide$app_hyb_installed_before_w3[ger_df_wide$sample_type.1 != "surveyonly" & !is.na(ger_df_wide$app_track_installed_before_w3)] <- ger_df_wide$app_track_installed_before_w3[ger_df_wide$sample_type.1 != "surveyonly" & !is.na(ger_df_wide$app_track_installed_before_w3)]

ger_df_wide$app_hyb_installed_between_w1w2[ger_df_wide$sample_type.1 != "surveyonly" & !is.na(ger_df_wide$app_track_installed_between_w1w2)] <- ger_df_wide$app_track_installed_between_w1w2[ger_df_wide$sample_type.1 != "surveyonly" & !is.na(ger_df_wide$app_track_installed_between_w1w2)]
ger_df_wide$app_hyb_installed_between_w2w3[ger_df_wide$sample_type.1 != "surveyonly" & !is.na(ger_df_wide$app_track_installed_between_w2w3)] <- ger_df_wide$app_track_installed_between_w2w3[ger_df_wide$sample_type.1 != "surveyonly" & !is.na(ger_df_wide$app_track_installed_between_w2w3)]


# correct incentivization treatment variables for tracking-only sample ------------

ger_df_wide$treat_incent.2[ger_df_wide$sample_type.1 == "trackingonly" & ger_df_wide$app_track_installed_before_w2 == TRUE] <- NA
ger_df_wide$treat_incent_eur1.2[ger_df_wide$sample_type.1 == "trackingonly" & ger_df_wide$app_track_installed_before_w2 == TRUE] <- NA
ger_df_wide$treat_incent_eur2.2[ger_df_wide$sample_type.1 == "trackingonly" & ger_df_wide$app_track_installed_before_w2 == TRUE] <- NA
ger_df_wide$treat_incent_eur5.2[ger_df_wide$sample_type.1 == "trackingonly" & ger_df_wide$app_track_installed_before_w2 == TRUE] <- NA
ger_df_wide$treat_incent_control.2[ger_df_wide$sample_type.1 == "trackingonly" & ger_df_wide$app_track_installed_before_w2 == TRUE] <- NA
ger_df_wide$treat_incent_any.2[ger_df_wide$sample_type.1 == "trackingonly" & ger_df_wide$app_track_installed_before_w2 == TRUE] <- NA



# standardize refined tracking indicators ------------

tracking_vars <- c(
  "app_track_installed_at_w1", "app_track_installed_at_w2", "app_track_installed_at_w3",
  "app_track_installed_before_w1", "app_track_installed_before_w2", "app_track_installed_before_w3",
  "app_track_installed_between_w1w2", "app_track_installed_between_w2w3", 
  "app_rep_installed_at_w1", "app_rep_installed_at_w2", "app_rep_installed_at_w3",
  "app_rep_installed_before_w1", "app_rep_installed_before_w2", "app_rep_installed_before_w3",
  "app_rep_installed_between_w1w2", "app_rep_installed_between_w2w3", 
  "app_hyb_installed_at_w1", "app_hyb_installed_at_w2", "app_hyb_installed_at_w3",
  "app_hyb_installed_before_w1", "app_hyb_installed_before_w2", "app_hyb_installed_before_w3", 
  "app_hyb_installed_between_w1w2", "app_hyb_installed_between_w2w3"
  )
  
for (i in tracking_vars){
  ger_df_wide[,paste0(i, "_std")] <- ger_df_wide[,i]/sd(ger_df_wide[,i][which(ger_df_wide$treat.1 == "control")], na.rm = TRUE)
  ger_df_wide[,paste0(i, "_incent_std")] <- ger_df_wide[,i]/sd(ger_df_wide[,i][which(ger_df_wide$treat_incent.2 == "Control")], na.rm = TRUE)
}


# prepare covariates for descriptives ---------------------

sociodem_vars <- c("age_cat5.1", "gender_cat2.1", "educ_cat3.1", "hhinc_cat3.1", "children_cat2.1")
sociodem_vars_labels <- c("Age", "Gender", "Education", "Household\nincome", "Children")

#risk_vars <- c("behav_pubtransport_cat2.1", "behav_restaurant_cat2.1", "behav_visitfriends_cat2.1", "aha_compliance_cat2.1", "precond_cat2.1", "covidcase_cat2.1", "urban_cat2.1", "cases90perc_cat2.1", "working_cat2.1")
#risk_vars_labels <- c("Public transport\nusage", "Restaurant/pub\nvisit", "Friend/family\nvisit", "NPI compliance", "Precondition", "COVID-19 case\nin network", "Lives in\nurban region", "Lives in high-\nincidence region", "Working")
risk_vars <- c("behav_pubtransport_cat2.1", "behav_restaurant_cat2.1", "behav_visitfriends_cat2.1", "aha_compliance_cat2.1", "precond_cat2.1", "covidcase_cat2.1", "urban_cat2.1", "cases90perc_cat2.1", "working_cat2.1")
risk_vars_labels <- c("Risk behavior:\nPublic transp. usage", "Risk behavior:\nRestaurant/pub visit", "Risk behavior:\nFriend/family visit", "NPI compliance", "Precondition", "COVID-19 case\nin network", "Lives in\nurban region", "Lives in high-\nincidence region", "Working")

motivation_vars <- c("trust_gov_cat3.1", "trust_sci_cat3.1", "trust_hcs_cat3.1",
                     "covidconcerned_self_cat2.1", "covidconcerned_famf_cat2.1",
                     "socresp_score_cat3.1", "selfint_score_cat3.1",
                     "datacontrol_score_cat3.1", "digliteracy_score_cat3.1")
motivation_vars_labels <- c("Trust in government", "Trust in science", "Trust in health\ncare system",
                            "COVID threat \nperception: self", "COVID threat\nperception: friends",
                            "Social responsibility", "Self interest",
                            "Need for\ndata control", "Digital literacy")

covars_df <- data.frame(variable = c(sociodem_vars, risk_vars, motivation_vars),
                        var_label = c(sociodem_vars_labels, risk_vars_labels, motivation_vars_labels),
                        category = c(rep("Sociodemographics", length(sociodem_vars)),
                                     rep("Risk status and behavior", length(risk_vars)),
                                     rep("Motivational factors", length(motivation_vars))),
                        stringsAsFactors = FALSE)



# prepare covariates for models ---------------------

# combine covariates
covars <- c(
  # demographics
  "age_cat.1", "female.1", "educ_cat.1", "children_num.1", "hhinc_cat.1",
  "urban.1", "at_work.1",
  # health
  "health_cond.1", "precond.1", 
  # trust
  "trust_gov.1", "trust_sci.1", "trust_hcs.1",
  # privacy and data attitudes and behavior
  "datacontrol_score.1", "digliteracy_score.1",
  # covid attitudes and experience
  "covidconcerned_self.1", "covidconcerned_famf.1", 
  "covidcase.1", "cases90perc.1", "riskybehav_score.1",
  "aha_compliance.1",
  # social responsibility, self interest
  "socresp_score.1", "selfint_score.1"
)

# combine covariates for incentivization model (wave 2 if available)
covars2 <- c(
  # demographics
  "age_cat.1", "female.1", "educ_cat.1", "children_num.1", "hhinc_cat.1",
  "urban.1", "at_work.1",
  # health
  "health_cond.1", "precond.1", 
  # trust
  "trust_gov.1", "trust_sci.1", "trust_hcs.1",
  # privacy and data attitudes and behavior
  "datacontrol_score.1", "digliteracy_score.1",
  # covid attitudes and experience
  "covidconcerned_self.2", "covidconcerned_famf.2", 
  "covidcase.1", "cases90perc.1", "riskybehav_score.2",
  "aha_compliance.2",
  # social responsibility, self interest
  "socresp_score.1", "selfint_score.1"
)

covars_red <- c(
  # demographics
  "age_cat.1", "female.1", "educ_cat.1", "children_num.1", "hhinc_cat.1", "at_work.1"
)




# prepare covariates for uptake model ---------------------

sociodem_vars <- c(
  # sociodemgraphics
  "age10.1", "female.1",
  "edu_mid.1", "edu_hi.1", 
  "inc_mid.1", "inc_hi.1",
  "children_yes.1"
)

risk_vars <- c(
  # risk factors and behavior
  "behav_pubtransport_num2.1", "behav_restaurant_num2.1", "behav_visitfriends_num2.1",
  "aha_compliance.1",
  "precond.1", "covidcase.1", "cases90perc.1", "urban.1", "at_work.1"
)

motivation_vars <- c(
  # motivational factors
  "trust_gov.1", "trust_sci.1", "trust_hcs.1",
  "covidconcerned_self.1", "covidconcerned_famf.1",  
  "socresp_score.1", "selfint_score.1",
   "datacontrol_score.1", "digliteracy_score.1"
)

sociodem_vars_labels <- c(
  # sociodemographics 
  "Age/10", "Gender:\nFemale", 
  "Education:\nMedium", "Education:\nHigh", 
  "Household income:\nMedium", "Household income:\nHigh", 
  "Children"
)
  # risk factors and behavior
risk_vars_labels <- c(
  "Public transport\nusage", "Restaurant/pub\nvisit", "Friend/family\nvisit",
  "NPI compliance",
  "Precondition", "COVID case in network", 
  "Lives in high COVID\nincidence region", "Lives in\nurban region", "Working"
)
  # motivational factors
motivation_vars_labels <- c(
  "Trust in government", "Trust in scientists", "Trust in HC system",
  "COVID threat perception:\nSelf", "COVID threat perception:\nFamily and Friends", 
  #"App attitudes", "App knowledge", 
  "Social responsibility", "Self interest",
  "Data control", "Digital literacy"
)

covars_adapted_df <- data.frame(variable = c(sociodem_vars, risk_vars, motivation_vars),
                                             var_label = c(sociodem_vars_labels, risk_vars_labels, motivation_vars_labels),
                                             category = c(rep("Sociodemographics", length(sociodem_vars)),
                                                          rep("Risk status and behavior", length(risk_vars)),
                                                          rep("Motivational factors", length(motivation_vars))),
                                             stringsAsFactors = FALSE)



# Save file -----------------------------------------------
saveRDS(ger_df_wide, file = "../data/clean/ger_df_wide.RDS")


# Save anonymized file ------------------------------------

ger_df_wide$id <- seq_len(nrow(ger_df_wide))
ger_df_wide_ids <- dplyr::select(ger_df_wide, psid.1, id)
write_csv(ger_df_wide_ids, file = "../data/psid.csv")

ger_df_wide <-  
  dplyr::select(ger_df_wide, 
                !starts_with("cj") & 
                  !starts_with("traits") & 
                  !starts_with("vcj") & 
                  !starts_with("vac") & 
                  !starts_with("zipcode") & 
                  !starts_with("BL") & 
                  !starts_with("Kreis") & 
                  !starts_with("attrs") & 
                  !starts_with("IPAddress") & 
                  !starts_with("Location")  & 
                  !starts_with("psid")  & 
                  !starts_with("device_id") & 
                  !starts_with("cases_per_100k") & 
                  !starts_with("ResponseId") &
                  !starts_with("RecipientLastName") &
                  !starts_with("RecipientFirstName") &
                  !starts_with("RecipientEmail") &
                  !starts_with("ExternalReference") &
                  !starts_with("DistributionChannel") &
                  !starts_with("UserLanguage") &
                  !starts_with("Typ"))
saveRDS(ger_df_wide, file = "../data/clean/ger_df_wide_an.RDS")


tracking_df_all <- merge(tracking_df_all, ger_df_wide_ids, by.x = "psid", by.y = "psid.1")
tracking_df_all <- dplyr::select(tracking_df_all, -psid, -device_id)
saveRDS(tracking_df_all, file = "../data/clean/tracking_df_all_an.RDS")


