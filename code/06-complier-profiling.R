### -----------------------------
### complier profiling
### -----------------------------

# load data --------------------------

ger_df_wide <- readRDS("../data/clean/ger_df_wide_an.RDS")



### Message experiment ----------------------

## run pro-social treatment models
dat <- filter(ger_df_wide, treat.1 != "selfinterest")

out_list <- list()
for (i in seq_along(covars_adapted)) {
  out_list[[i]] <- with(dat, ivdesc(X = eval(parse(text = covars_adapted[i])), 
                                    D = as.numeric(treat_comp.1), 
                                    Z = as.numeric(treat_prosoc.1),
                        boot = TRUE, bootn = 3000)) %>%
    as.data.frame()
  out_list[[i]]$covar <- covars_adapted[i]
  out_list[[i]]$covar_label <-  covars_labels[i]
  print(paste0(i, "..."))
}
out_df_prosocial <- bind_rows(out_list)
out_df_prosocial$treatment <- "Pro-social"


## run self-interest treatment models
dat <- filter(ger_df_wide, treat.1 != "prosocial")

out_list <- list()
for (i in seq_along(covars_adapted)) {
  out_list[[i]] <- with(dat, ivdesc(X = eval(parse(text = covars_adapted[i])), 
                                    D = as.numeric(treat_comp.1), 
                                    Z = as.numeric(treat_selfint.1),
                                    boot = TRUE, bootn = 3000)) %>%
    as.data.frame()
  out_list[[i]]$covar <- covars_adapted[i]
  out_list[[i]]$covar_label <- covars_labels[i]
  print(paste0(i, "..."))
}
out_df_selfinterest <- bind_rows(out_list)
out_df_selfinterest$treatment <- "Self-interest"


## plot outcomes

# join data frames
out_df <- bind_rows(out_df_prosocial, out_df_selfinterest)
out_df <- filter(out_df, group != "at")

# re-define group variable
out_df$group_label <- recode_factor(out_df$group, sample = "Sample", co = "Compliers", nt = "Never-takers")
out_df$covar_label <- factor(out_df$covar_label, levels = covars_labels)

# plot
p <- ggplot(out_df, aes(mu, group_label, color = treatment)) + 
  geom_pointrange(aes(xmin = mu - 1.96*mu_se, xmax = mu + 1.96*mu_se), shape = 19, fatten = 1, size = 0.75, stat="identity", position = position_dodge(0.5)) + 
  geom_pointrange(aes(xmin = mu - 1*mu_se, xmax = mu + 1*mu_se), shape = 19, fatten = 1, size = 1, stat="identity", position = position_dodge(0.5)) + 
  facet_rep_wrap(~ covar_label,  scales='free_x', repeat.tick.labels = "bottom", ncol = 5) +
  scale_color_manual(values = c("red", "blue")) + 
  labs(y = "", x = substitute(x, list(x = "\nMean and 68%/95% confidence intervals"))) + 
  theme(panel.background = element_blank(), 
        axis.title.y = element_blank(), 
        axis.title.x = element_text(hjust = 0.5, size = 12), 
        axis.text = element_text(size = 12), 
        plot.caption = element_text(size = 7), 
        strip.text.y = element_text(size = 12, colour = "black"),
        plot.margin=unit(c(0.1,0.1,0.1,0.1), "cm"),
        panel.spacing.x = unit(-1.5, "cm"),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.key.size = unit(1, "line"),
        panel.border = element_rect(colour = "black", fill = NA))


pdf(file="../figures/message-complier-profiling-main.pdf", height = 12, width = 10)
p
dev.off()





### Incentivization experiment ----------------------

crosstab(filter(ger_df_wide, treat_incent_agree.2 == TRUE), row.vars = "app_hyb_installed_at_w3", col.vars = "treat_incent.2", type = c("c"), style = "long", addmargins = FALSE)

## run pooled treatment models
dat <- filter(ger_df_wide, !is.na(treat_incent_any.2) & sample_type.2 != "trackingonly")

out_list <- list()
for (i in seq_along(covars_adapted_df$variable)) {
  out_list[[i]] <- with(dat, ivdesc(X = eval(parse(text = covars_adapted_df$variable[i])), 
                                    D = as.numeric(app_hyb_installed_at_w3), 
                                    Z = as.numeric(treat_incent_any.2),
                                    boot = TRUE, bootn = 3000)) %>%
    as.data.frame()
  out_list[[i]]$covar <- covars_adapted_df$variable[i]
  out_list[[i]]$covar_label <-  covars_adapted_df$var_label[i]
  print(paste0(i, "..."))
}
out_df_pooled <- bind_rows(out_list)
out_df_pooled$treatment <- "Incentivization (pooled)"


## run EUR 1 treatment models

dat <- filter(ger_df_wide, !(treat_incent.2 %in% c("EUR 2", "EUR 5")) & sample_type.2 != "trackingonly")

out_list <- list()
for (i in seq_along(covars_adapted_df$variable)) {
  out_list[[i]] <- with(dat, ivdesc(X = eval(parse(text = covars_adapted_df$variable[i])), 
                                    D = as.numeric(app_hyb_installed_at_w3), 
                                    Z = as.numeric(treat_incent_eur1.2),
                                    boot = TRUE, bootn = 3000)) %>%
    as.data.frame()
  out_list[[i]]$covar <- covars_adapted_df$variable[i]
  out_list[[i]]$covar_label <-  covars_adapted_df$var_label[i]
  print(paste0(i, "..."))
}
out_df_eur1 <- bind_rows(out_list)
out_df_eur1$treatment <- "EUR 1"

## run EUR 2 treatment models

dat <- filter(ger_df_wide, !(treat_incent.2 %in% c("EUR 1", "EUR 5")) & sample_type.2 != "trackingonly")

out_list <- list()
for (i in seq_along(covars_adapted_df$variable)) {
  out_list[[i]] <- with(dat, ivdesc(X = eval(parse(text = covars_adapted_df$variable[i])), 
                                    D = as.numeric(app_hyb_installed_at_w3), 
                                    Z = as.numeric(treat_incent_eur2.2),
                                    boot = TRUE, bootn = 3000)) %>%
    as.data.frame()
  out_list[[i]]$covar <- covars_adapted_df$variable[i]
  out_list[[i]]$covar_label <-  covars_adapted_df$var_label[i]
  print(paste0(i, "..."))
}
out_df_eur2 <- bind_rows(out_list)
out_df_eur2$treatment <- "EUR 2"

## run EUR 5 treatment models

dat <- filter(ger_df_wide, !(treat_incent.2 %in% c("EUR 1", "EUR 2")) & sample_type.2 != "trackingonly")

out_list <- list()
for (i in seq_along(covars_adapted_df$variable)) {
  out_list[[i]] <- with(dat, ivdesc(X = eval(parse(text = covars_adapted_df$variable[i])), 
                                    D = as.numeric(app_hyb_installed_at_w3), 
                                    Z = as.numeric(treat_incent_eur5.2),
                                    boot = TRUE, bootn = 3000)) %>%
    as.data.frame()
  out_list[[i]]$covar <- covars_adapted_df$variable[i]
  out_list[[i]]$covar_label <-  covars_adapted_df$var_label[i]
  print(paste0(i, "..."))
}
out_df_eur5 <- bind_rows(out_list)
out_df_eur5$treatment <- "EUR 5"


## plot outcomes by treatment group

# join data frames
out_df <- bind_rows(out_df_eur1, out_df_eur2, out_df_eur5)

# re-define group variable
out_df$group_label <- recode_factor(out_df$group, sample = "Sample", co = "Compliers", nt = "Never-takers", at = "Always-takers")
out_df$covar_label <- factor(out_df$covar_label, levels = covars_adapted_df$var_label)

# plot
p <- ggplot(out_df, aes(mu, group_label, color = treatment)) + 
  geom_pointrange(aes(xmin = mu - 1.96*mu_se, xmax = mu + 1.96*mu_se), shape = 19, fatten = 1, size = 0.75, stat="identity", position = position_dodge(0.5)) + 
  geom_pointrange(aes(xmin = mu - 1*mu_se, xmax = mu + 1*mu_se), shape = 19, fatten = 1, size = 1, stat="identity", position = position_dodge(0.5)) + 
  facet_rep_wrap(~ covar_label,  scales='free_x', repeat.tick.labels = "bottom", ncol = 5) +
  scale_color_manual(values = c("#99d8c9", "#41ae76", "#005824")) + 
  labs(y = "", x = substitute(x, list(x = "\nMean and 68%/95% confidence intervals"))) + 
  theme(panel.background = element_blank(), 
        axis.title.y = element_blank(), 
        axis.title.x = element_text(hjust = 0.5, size = 12), 
        axis.text = element_text(size = 12), 
        plot.caption = element_text(size = 7), 
        strip.text.y = element_text(size = 12, colour = "black"),
        plot.margin=unit(c(0.1,0.1,0.1,0.1), "cm"),
        panel.spacing.x = unit(-2.5, "cm"),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.key.size = unit(1, "line"),
        panel.border = element_rect(colour = "black", fill = NA))


pdf(file="../figures/incentivization-complier-profiling-treatments.pdf", height = 12, width = 10)
p
dev.off()



## plot outcomes, pooled treatment

# re-define group variable
out_df_pooled$group_label <- recode_factor(out_df_pooled$group, sample = "Sample", co = "Compliers", nt = "Never-takers", at = "Always-takers")
out_df_pooled$covar_label <- factor(out_df_pooled$covar_label, levels = covars_adapted_df$var_label)

# plot
p <- ggplot(out_df_pooled, aes(mu, group_label)) + 
  geom_pointrange(aes(xmin = mu - 1.96*mu_se, xmax = mu + 1.96*mu_se), shape = 19, fatten = 1, size = 0.75, stat="identity", position = position_dodge(0.5)) + 
  geom_pointrange(aes(xmin = mu - 1*mu_se, xmax = mu + 1*mu_se), shape = 19, fatten = 1, size = 1, stat="identity", position = position_dodge(0.5)) + 
  facet_rep_wrap(~ covar_label,  scales='free_x', repeat.tick.labels = "bottom", ncol = 5) +
  scale_color_manual(values = c("black")) + 
  labs(y = "", x = substitute(x, list(x = "\nMean and 68%/95% confidence intervals"))) + 
  theme(panel.background = element_blank(), 
        axis.title.y = element_blank(), 
        axis.title.x = element_text(hjust = 0.5, size = 12), 
        axis.text = element_text(size = 12), 
        plot.caption = element_text(size = 7), 
        strip.text.y = element_text(size = 12, colour = "black"),
        plot.margin=unit(c(0.1,0.1,0.1,0.1), "cm"),
        panel.spacing.x = unit(-2.5, "cm"),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.key.size = unit(1, "line"),
        panel.border = element_rect(colour = "black", fill = NA))


pdf(file="../figures/incentivization-complier-profiling-pooled.pdf", height = 12, width = 10)
p
dev.off()

