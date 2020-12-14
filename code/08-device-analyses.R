### -----------------------------
### device analyses
### -----------------------------

# load data --------------------------

apple_remove <- FALSE
tracking_period <- "oct" # "study", "full", "oct"

# full tracking period
if(tracking_period == "full") {
ger_df_wide <- readRDS("../data/clean/ger_df_wide_apple_full_an.RDS")
tracking_df_all <- readRDS("../data/clean/tracking_df_all_apple_full_an.RDS")
}

if(tracking_period == "oct"){
ger_df_wide <- readRDS("../data/clean/ger_df_wide_apple_oct_an.RDS")
tracking_df_all <- readRDS("../data/clean/tracking_df_all_apple_oct_an.RDS")
}



ger_df_wide <- readRDS("../data/clean/ger_df_wide_an.RDS")
# device type vs. tracked usage at end
descr::CrossTable(ger_df_wide$device_os.1, ger_df_wide$app_rep_installed_before_w1,
                  prop.c = TRUE, prop.t = FALSE, prop.chisq = FALSE,
                  dnn = c("Operating System", "Reported App usage"),
                  digits = 2)



# apple vs. android users balance tables --------------

# compute covariate means by treatment group
covar_means_df <- ger_df_wide[,c("apple.1", covars_adapted_df$variable)] %>% group_by(apple.1) %>% summarise_all(list(~ mean(.x, na.rm = TRUE))) %>% filter(!is.na(apple.1))
apple_means_df <- filter(covar_means_df, apple.1 == TRUE) %>% pivot_longer(-apple.1, names_to = "variable", values_to = "apple_mean") %>% dplyr::select(-apple.1)
android_means_df <- filter(covar_means_df, apple.1 == FALSE) %>% pivot_longer(-apple.1, names_to = "variable", values_to = "android_mean") %>% dplyr::select(-apple.1)

# combine into table, compute difference + p-values
apple_android_means_df <- left_join(apple_means_df, android_means_df, by = "variable")
apple_android_means_df$variable_label <- str_replace(covars_adapted_df$var_label, "\\n", " ")
apple_android_means_df$difference_app_and = apple_android_means_df$apple_mean - apple_android_means_df$android_mean
apple_android_means_df$p_value <- map_dbl(covars_adapted_df$variable, t_test_out, "apple.1", ger_df_wide)
apple_android_means_df <- dplyr::select(apple_android_means_df, variable_label, apple_mean, android_mean, difference_app_and, p_value)
names(apple_android_means_df) <- c("Covariate", "Apple", "Android", "Diff. (Apple-Android)", "p value")

# export table
covar_balance_apple_androidxtab <- xtable(apple_android_means_df, digits = 2, label = "tab:device-balance")
caption(covar_balance_apple_androidxtab) <- "Covariate balance: Apple vs. Android device (means reported)"
covar_balance_apple_androidxtab$`p value` <- minsig_fixer(covar_balance_apple_androidxtab$`p value`)
print(covar_balance_apple_androidxtab, type = "latex", size = "footnotesize", table.placement = "h!", include.rownames = FALSE, include.colnames = TRUE, caption.placement = "top", file = "../figures/device_balance_table.tex")




# cumulative time-series of first app usage, together with survey timings -------------------

# field times
(fieldtime_w1 <- range(ger_df_wide$StartDate.1, na.rm = TRUE))
(fieldtime_w2 <- range(ger_df_wide$StartDate.2, na.rm = TRUE))
(fieldtime_w3 <- range(ger_df_wide$StartDate.3, na.rm = TRUE))

# identify unique obs
tracking_df_unique <- distinct(tracking_df_all, id, .keep_all = TRUE) 
tracking_df_unique$apple <- tracking_df_unique$device_manufacturer == "Apple"


# merge treatment status to tracking data
treat_df <- dplyr::select(ger_df_wide, id, treat.1, treat_incent.2, app_install.2, wave.1, wave.2, wave.3)
tracking_df_unique <- merge(tracking_df_unique, treat_df, by.x = "id", by.y = "id")

# compute cumulative time series

# set time period
if(tracking_period == "full") {
  start_date <- "2020/06/16"
    end_date <- "2020/10/31"
}
if(tracking_period == "oct"){
  start_date <- "2020/10/01"
    end_date <- "2020/10/31"
}

# function to generate ts
generate_ts <- function(data, start_date = "2020/06/16", end_date = "2020/09/22") { 
  install_dates <- sort(data[,"track_date_first_install"][data[,"track_date_first_install"] != as.Date("9999-01-01")])
  dates <- seq(as.Date(start_date), as.Date(end_date), by = "day") 
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


# generate ts
tracking_ts_surveyplus_apple <- generate_ts(filter(tracking_df_unique, apple == TRUE & survey_indicator == "survey"), start_date = start_date, end_date = end_date)
tracking_ts_trackingonly_apple <- generate_ts(filter(tracking_df_unique, apple == TRUE & survey_indicator == "no survey"), start_date = start_date, end_date = end_date)
tracking_ts_surveyplus_noapple <- generate_ts(filter(tracking_df_unique, apple == FALSE & survey_indicator == "survey"), start_date = start_date, end_date = end_date)
tracking_ts_trackingonly_noapple <- generate_ts(filter(tracking_df_unique, apple == FALSE & survey_indicator == "no survey"), start_date = start_date, end_date = end_date)



# plot by device manufacturer, full tracking period
if(tracking_period == "full"){
pdf(file="../figures/app-usage-tracking-timeseries-manufacturer.pdf", height = 5, width = 9, family="Helvetica")
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
}



# plot by device manufacturer, after study period
if(tracking_period == "oct"){
pdf(file="../figures/app-usage-tracking-timeseries-manufacturer-after-study.pdf", height = 5, width = 9, family="Helvetica")
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
}




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





# compute ITT for apple/android users for takeup in October -------------------

ger_df_wide <- readRDS("../data/clean/ger_df_wide_apple_oct_an.RDS")
tracking_df_all <- readRDS("../data/clean/tracking_df_all_apple_oct_an.RDS")
tracking_df_unique <- distinct(tracking_df_all, id, .keep_all = TRUE) 
tracking_df_unique$apple <- tracking_df_unique$device_manufacturer == "Apple"

ger_df_wide <- left_join(ger_df_wide, 
                         dplyr::select(tracking_df_unique, id, app_track_installed_at_end = track_installed),
                         by = "id")


# device type vs. tracked usage at end
descr::CrossTable(ger_df_wide$device_os.1, ger_df_wide$app_track_installed_at_end,
                  prop.c = TRUE, prop.t = FALSE, prop.chisq = FALSE,
                  dnn = c("Operating System", "Tracked App usage"),
                  digits = 2)


# standardized outcome var
i <- "app_track_installed_at_end"
ger_df_wide[,paste0(i, "_incent_std")] <- ger_df_wide[,i]/sd(ger_df_wide[,i][which(ger_df_wide$treat_incent.2 == "Control")], na.rm = TRUE)
table(ger_df_wide$app_track_installed_at_end)
table(ger_df_wide$app_track_installed_at_end_incent_std)

# define vars
dv <- "app_track_installed_at_end"
dv_pre <- "app_track_installed_at_w2"
dv_label <- "Uptake (tracked)"
dat_surveytrackingplus <- filter(ger_df_wide, sample_type.1 == "surveytracking")


# both device types
pooled_std <- run_models(trt = "treat_incent_any.2", dv = "app_track_installed_at_end_incent_std", dv_pre = dv_pre, covars = covars_red, data = filter(dat_surveytrackingplus, !is.na(treat_incent_any.2)), run.cace = FALSE)
pooled <- run_models(trt = "treat_incent_any.2", dv = "app_track_installed_at_end", dv_pre = dv_pre, covars = covars_red, data = filter(dat_surveytrackingplus, !is.na(treat_incent_any.2)), run.cace = FALSE)

# apple
apple_std <- h_incent_2a_pooled_apple <- run_models(trt = "treat_incent_any.2", dv = "app_track_installed_at_end_incent_std", dv_pre = dv_pre, covars = covars_red, data = filter(dat_surveytrackingplus, !is.na(treat_incent_any.2), apple.1 == TRUE), run.cace = FALSE)
apple <- h_incent_2a_pooled_apple <- run_models(trt = "treat_incent_any.2", dv = "app_track_installed_at_end", dv_pre = dv_pre, covars = covars_red, data = filter(dat_surveytrackingplus, !is.na(treat_incent_any.2), apple.1 == TRUE), run.cace = FALSE)

# android
android_std <- run_models(trt = "treat_incent_any.2", dv = "app_track_installed_at_end_incent_std", dv_pre = dv_pre, covars = covars_red, data = filter(dat_surveytrackingplus, !is.na(treat_incent_any.2), apple.1 == FALSE), run.cace = FALSE)
android <- run_models(trt = "treat_incent_any.2", dv = "app_track_installed_at_end", dv_pre = dv_pre, covars = covars_red, data = filter(dat_surveytrackingplus, !is.na(treat_incent_any.2), apple.1 == FALSE), run.cace = FALSE)

format_latex(pooled_std, dv_label, trt_label = "Pooled vs. control", path = "../figures/itt_oct_pooled.tex", add.cace = FALSE)
format_latex(apple_std, dv_label, trt_label = "Pooled vs. control, Apple users", path = "../figures/itt_oct_apple.tex", add.cace = FALSE)
format_latex(android_std, dv_label, trt_label = "Pooled vs. control, Android users", path = "../figures/itt_oct_android.tex", add.cace = FALSE)




